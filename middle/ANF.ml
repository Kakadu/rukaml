(* https://www.cs.swarthmore.edu/~jpolitz/cs75/s16/n_anf-tutorial.html *)

type config =
  { mutable log_enabled : bool
  ; mutable opt_arity_inline : bool
  ; mutable opt_cmp_into_if_inline : bool
  }

let cfg = { log_enabled = false; opt_arity_inline = true; opt_cmp_into_if_inline = true }
let disable_arity_inline () = cfg.opt_arity_inline <- false
let disable_cmp_into_if_inline () = cfg.opt_cmp_into_if_inline <- false
let set_logging b = cfg.log_enabled <- b

let log fmt =
  if cfg.log_enabled
  then Format.kasprintf (Format.printf "%s\n%!") fmt
  else Format.ifprintf Format.std_formatter fmt
;;

let failwiths fmt = Format.kasprintf failwith fmt

open Frontend

type apat = APname of Ident.t [@@deriving show { with_path = false }]

type imm_expr =
  | AUnit
  | AConst of Parsetree.const
  | AVar of Ident.t
  | APrimitive of string * int
  | ATuple of imm_expr * imm_expr * imm_expr list
  | AConstruct of int * imm_expr list
  | AArray of imm_expr list
  | ALam of apat * expr

(* TODO(Kakadu): array, lambda, constructor and tuple are not immediates *)
and c_expr =
  | CApp of imm_expr * imm_expr * imm_expr list
  | CIte of c_expr * expr * expr
  | CAtom of imm_expr

and expr =
  | ELet of Parsetree.rec_flag * Typedtree.pattern * c_expr * expr
  (* Maybe recursive flag is not required? *)
  | EComplex of c_expr
[@@deriving show { with_path = false }]

type vb = Parsetree.rec_flag * Ident.t * expr
(* TODO: only complex expression should be there *)

let complex_of_atom x = EComplex (CAtom x)
let ecomplex x = EComplex x
let make_let_nonrec name rhs wher = ELet (NonRecursive, Typedtree.Tpat_var name, rhs, wher)
let catom i = CAtom i
let cvar name = CAtom (AVar name)
let cite cond th el = CIte (cond, th, el)
let alam name e = ALam (APname name, e)
let elam name e = complex_of_atom (alam name e)
let elet flg pat cexp exp = ELet (flg, pat, cexp, exp)

let group_abstractions =
  let rec helper acc = function
    | EComplex (CAtom (ALam (p, e))) -> helper (p :: acc) e
    | (EComplex _ | ELet _) as e -> List.rev acc, e
  in
  helper []
;;

[@@@ocaml.warnerror "-11"]

let is_infix_binop = function
  | "=" | "+" | "-" | "*" | "/" | "<" | "<=" | ">" | ">=" -> true
  | _ -> false
;;

let pp_comma_list eta =
  Format.pp_print_list ~pp_sep:(fun ppf () -> Format.fprintf ppf ", ") eta
;;

(** Formatting *)
include struct
  open Format

  let pp_apat ppf = function
    | APname s -> Ident.pp ppf s
  ;;

  let rec helper ppf = function
    | ELet (flg, name, CAtom (ALam (arg1, rhs)), wher) ->
      fprintf
        ppf
        "@[<v 2>@[<hov 2>@[let %a%a %a =@]@ "
        Pprint.pp_flg
        flg
        Pprinttyped.pp_pattern
        name
        pp_apat
        arg1;
      fprintf ppf "@[%a@]@ in@]@ @[%a@]@]" helper rhs helper wher
    | ELet (_, name, rhs, wher) ->
      fprintf
        ppf
        "@[<v 2>@[let %a = %a in@]@ @[%a@]@]"
        Pprinttyped.pp_pattern
        name
        helper_c
        rhs
        helper
        wher
    | EComplex (CAtom (AConst c)) -> Pprint.pp_const ppf c
    | EComplex ea -> helper_c ppf ea

  and helper_c ppf = function
    | CApp (APrimitive (binop, _arity), arg1, [ arg2 ]) when is_infix_binop binop ->
      fprintf ppf "(%a %s %a)" helper_a arg1 binop helper_a arg2
    | CApp (f, arg1, args) ->
      fprintf
        ppf
        "@[%a %a %a@]"
        helper_a
        f
        helper_a
        arg1
        (pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf " ") helper_a)
        args
    | CAtom a -> helper_a ppf a
    | CIte (acond, th, el) ->
      fprintf
        ppf
        "@[<v>@[(if %a@]@ @[then %a@]@ @[else %a)@]@]"
        helper_c
        acond
        helper
        th
        helper
        el

  and helper_a ppf = function
    | ALam (arg1, EComplex (CAtom (ALam (arg2, EComplex (CAtom (ALam (arg3, e))))))) ->
      fprintf
        ppf
        "@[(fun %a %a %a -> %a)@]"
        pp_apat
        arg1
        pp_apat
        arg2
        pp_apat
        arg3
        helper
        e
    | ALam (arg1, EComplex (CAtom (ALam (arg2, e)))) ->
      fprintf ppf "@[(fun %a %a -> %a)@]" pp_apat arg1 pp_apat arg2 helper e
    | ALam (name, e) -> fprintf ppf "(fun %a -> %a)" pp_apat name helper e
    | AConst c -> Pprint.pp_const ppf c
    | APrimitive (s, _arity) -> fprintf ppf "%s" s
    | AVar s -> Ident.pp ppf s
    | ATuple (a, b, ts) -> fprintf ppf "@[(%a)@]" (pp_comma_list helper_a) (a :: b :: ts)
    | AArray xs -> fprintf ppf "@[[|%a|]@]" (pp_comma_list helper_a) xs
    | AUnit -> fprintf ppf "()"
    | AConstruct (id, []) -> fprintf ppf "Constr_%d" id
    | AConstruct (id, [ arg ]) -> fprintf ppf "@[(Constr_%d %a)@]" id helper_a arg
    | AConstruct (id, args) ->
      fprintf ppf "@[(Constr_%d (%a))@]" id (pp_comma_list helper_a) args
  ;;

  let pp_a = helper_a
  let pp_c = helper_c
  let pp = helper

  let group_abstractions =
    let rec helper acc = function
      | EComplex (CAtom (ALam (pat, body))) -> helper (pat :: acc) body
      | e -> List.rev acc, e
    in
    helper []
  ;;

  let pp_vb ppf (flg, name, expr) =
    let pats, body = group_abstractions expr in
    fprintf ppf "@[<v 2>@[let %a%a " Pprint.pp_flg flg Ident.pp name;
    List.iter (fprintf ppf "%a " pp_apat) pats;
    fprintf ppf "=@]@ @[%a@]@]" pp body
  ;;

  let pp_stru ppf xs = fprintf ppf "@[<v>%a@]" (pp_print_list pp_vb) xs
end

let used_once_as_function ~where name =
  (* This test allows us to inline partial applications into bigger once.
     We only can do this correctly, if var is used only once, and as a function
     (The last part was not trivial) *)
  let used = ref 0 in
  let used_as_fun = ref 0 in
  let rec helper_c = function
    | CIte (ccond, ethen, eelse) ->
      helper_c ccond;
      helper ethen;
      helper eelse
    | CApp (AVar f, arg1, args) when Ident.equal name f ->
      incr used_as_fun;
      helper_i arg1;
      List.iter helper_i args
    | CApp (f, arg1, args) ->
      helper_i f;
      helper_i arg1;
      List.iter helper_i args
    | CAtom i -> helper_i i
  and helper_i = function
    | AVar id when Ident.equal id name -> incr used
    | APrimitive _ | AUnit | AConst _ | AVar _ -> ()
    | ALam (_, e) -> helper e
    | AArray xs -> List.iter helper_i xs
    | ATuple (a, b, cs) ->
      helper_i a;
      helper_i b;
      List.iter helper_i cs
    | AConstruct (_, args) -> List.iter helper_i args
  and helper : expr -> unit = function
    | EComplex c -> helper_c c
    | ELet (_, _path, cexpr, expr) ->
      (* TODO: support hiding *)
      helper_c cexpr;
      helper expr
  in
  helper where;
  (* TODO(Kakadu): Maybe 0 is OK too? *)
  !used = 0 && !used_as_fun = 1
;;

let used_once_in_if ~where name =
  let used_in_if = ref 0 in
  let used = ref 0 in
  (* let rec is_not_used *)
  let rec helper_c = function
    | CIte (CAtom (AVar id), ethen, eelse) when Ident.equal id name ->
      incr used_in_if;
      helper ethen;
      helper eelse
    | CIte (_, ethen, eelse) ->
      helper ethen;
      helper eelse
    | CApp (f, arg1, args) ->
      helper_i f;
      helper_i arg1;
      List.iter helper_i args
    | CAtom (AVar id) when Ident.equal id name -> incr used
    | CAtom i -> helper_i i
  and helper_i = function
    | AVar id when Ident.equal id name -> incr used
    | APrimitive _ | AUnit | AConst _ | AVar _ -> ()
    | ALam (_, e) -> helper e
    | AArray xs -> List.iter helper_i xs
    | ATuple (a, b, cs) ->
      helper_i a;
      helper_i b;
      List.iter helper_i cs
    | AConstruct (_, args) -> List.iter helper_i args
  and helper : expr -> unit = function
    | EComplex c -> helper_c c
    | ELet (_, _path, cexpr, expr) ->
      (* TODO: support hiding *)
      helper_c cexpr;
      helper expr
  in
  helper where;
  !used_in_if = 1 && !used = 0
;;

(* TODO: Why we substitute by complex expression only? *)
let substitute ~where ident1 (rhs : c_expr) : expr =
  let rec helper = function
    | EComplex c -> ecomplex (helper_c c)
    | ELet (flg, pat, cexpr, expr) -> elet flg pat (helper_c cexpr) (helper expr)
  and helper_c = function
    | CAtom (AVar x) when Ident.equal x ident1 -> rhs
    | CAtom (AVar _) as c -> c
    | CAtom i -> catom (helperi i)
    | CIte (CAtom (AVar name), ethen, eelse) when Ident.equal ident1 name ->
      cite rhs (helper ethen) (helper eelse)
    | CIte (cond, ethen, eelse) -> cite (helper_c cond) (helper ethen) (helper eelse)
    | CApp (AVar x, arg1, args) when Ident.equal x ident1 ->
      (match rhs with
       | CApp (f, arg0, arg_mid) -> CApp (f, arg0, arg_mid @ (arg1 :: args))
       | _ -> assert false)
    | ( CApp ((APrimitive _ as _f), _arg1, _args)
      | CApp ((AVar _ as _f), _arg1, _args) (* not ident1 *) ) as is ->
      (* TODO: Do we  need to lookup inside args? *)
      is
    | c ->
      Format.eprintf "%a\n%!" pp_c c;
      Format.eprintf "can't substitute %a -> %a\n%!" Ident.pp ident1 pp_c rhs;
      assert false
  and helperi = function
    | AConst (PConst_bool true) -> AConst (PConst_int 1)
    | AConst (PConst_bool false) -> AConst (PConst_int 0)
    | (ATuple _ | APrimitive _ | AConst _ | AUnit) as i -> i
    | AConstruct (tag, is) -> AConstruct (tag, List.map helperi is)
    | AVar name when Ident.equal ident1 name ->
      Format.eprintf "Possible missing substitution. %s %d\n%!" __FILE__ __LINE__;
      AVar name
    | AVar _ as i -> i
    | i ->
      Format.eprintf "%a\n%!" pp_a i;
      failwiths "%s: unsupported case" __FUNCTION__
  in
  let ans = helper where in
  log
    "@[<v>@[  %a |-> %a@]@ @[%a@] ~~~> @[%a@]@]"
    Ident.pp
    ident1
    pp_c
    rhs
    pp
    where
    pp
    ans;
  ans
;;

let%expect_test _ =
  let vx = Ident.of_string "x" in
  let v7 = Ident.of_string "v7" in
  let v9 = Ident.of_string "v9" in
  let vf = Ident.of_string "vf" in
  let ans = EComplex (CAtom (AVar vx)) in
  Format.printf "%a\n" pp ans;
  [%expect {| x |}];
  Format.printf "%a\n" pp
  @@ substitute
       vx
       (CApp (AVar vf, AConst (Parsetree.PConst_int 1), []))
       ~where:
         (elet
            NonRecursive
            (Typedtree.Tpat_var v7)
            (CAtom (AVar vf))
            (EComplex (CApp (AVar vx, AVar v9, []))));
  [%expect
    {|
    let v7 = vf in
      vf 1 v9 |}]
;;

module Arity_map = struct
  include Map.Make (String)

  let is_under ident arity acc =
    match find ident.Ident.hum_name acc with
    | exception Not_found -> false
    | x -> x > arity
  ;;
end

let simplify : _ Arity_map.t -> expr -> expr =
  let is_comparison : c_expr -> bool = function
    | CApp (APrimitive (("<" | "=" | "<="), _), _, [ _ ]) ->
      (* TODO(Kakadu): fix here, when we get user-defined operators *)
      true
    | _ -> false
  in
  let rec helper_a acc = function
    | ALam (name, e) -> ALam (name, helper acc e)
    | x -> x
  and helper_c acc e =
    let rez =
      match e with
      | CAtom a -> CAtom (helper_a acc a)
      | CApp (f, arg1, args) ->
        CApp (helper_a acc f, helper_a acc arg1, List.map (helper_a acc) args)
      | CIte (cond, th, el) -> CIte (helper_c acc cond, helper acc th, helper acc el)
    in
    (* log "Simpl_c: @[%a@] ~~> @[%a@] " pp_c e pp_c rez; *)
    rez
  and helper acc e =
    let rez =
      match e with
      | EComplex e -> EComplex (helper_c acc e)
      | ELet
          ( Parsetree.NonRecursive
          , Tpat_var name1
          , (CApp (AVar fname, _arg1, args) as rhs)
          , where_ )
        when used_once_as_function name1 ~where:where_
             && Arity_map.is_under fname (1 + List.length args) acc
             && cfg.opt_arity_inline -> helper acc (substitute ~where:where_ name1 rhs)
      | ELet (Parsetree.NonRecursive, Tpat_var name1, body, EComplex (CAtom (AVar name2)))
        when Ident.equal name1 name2 ->
        (* let x = x in ... *)
        EComplex (helper_c acc body)
      | ELet
          ( NonRecursive
          , Tpat_var name1
          , body
          , ELet (NonRecursive, var2, CAtom (AVar name2), wher_) )
        when Ident.equal name1 name2 ->
        (* let name1 = ... in
           let name1 = ... in *)
        helper acc (ELet (NonRecursive, var2, body, wher_))
      | ELet (NonRecursive, Tpat_var v1, rhs, where)
        when used_once_in_if v1 ~where && is_comparison rhs && cfg.opt_cmp_into_if_inline
        -> helper acc (substitute ~where v1 rhs)
      | ELet (flg, name, body, wher) ->
        ELet (flg, name, helper_c acc body, helper acc wher)
    in
    (* log "Simpl: @[%a@] ~~> @[%a@] " pp e pp rez; *)
    rez
  in
  fun acc e ->
    (* log "Simplification of @[%a@]" pp e; *)
    helper acc e
;;

let%expect_test _ =
  let ex1 =
    let temp1_id = Ident.of_string "temp1" in
    let f_id = Ident.of_string "f" in
    let x_id = Ident.of_string "x" in
    make_let_nonrec
      temp1_id
      (CAtom (alam f_id (complex_of_atom (alam x_id (complex_of_atom (AVar x_id))))))
      (complex_of_atom (AVar temp1_id))
  in
  Format.printf "%a\n~~>\n%a\n%!" pp ex1 pp (simplify Arity_map.empty ex1);
  [%expect
    {|
  let temp1 f = (fun x -> x) in
    temp1
  ~~>
  (fun f x -> x) |}]
;;

let%expect_test _ =
  let ex1 =
    let temp1_id = Ident.of_string "temp1" in
    let temp2_id = Ident.of_string "temp2" in
    let f_id = Ident.of_string "f" in
    let x_id = Ident.of_string "x" in
    make_let_nonrec
      temp1_id
      (CAtom
         (alam
            f_id
            (make_let_nonrec
               temp2_id
               (CAtom (alam x_id (complex_of_atom (AVar x_id))))
               (complex_of_atom (AVar temp2_id)))))
      (complex_of_atom (AVar temp1_id))
  in
  Format.printf "%a\n~~>\n%a\n%!" pp ex1 pp (simplify Arity_map.empty ex1);
  [%expect
    {|
    let temp1 f = let temp2 x = x in
                    temp2 in
      temp1
    ~~>
    (fun f x -> x) |}]
;;

let simplify_vb acc (flag, name, body) =
  let get_arity x =
    match group_abstractions x with
    | [], _ -> 0
    | xs, _ -> List.length xs
  in
  match flag, name.Ident.hum_name with
  | Parsetree.Recursive, s ->
    let arity = get_arity body in
    let new_acc = Arity_map.add s arity acc in
    new_acc, (flag, name, simplify new_acc body)
  | NonRecursive, s ->
    let arity = get_arity body in
    let new_acc = Arity_map.add s arity acc in
    new_acc, (flag, name, simplify acc body)
;;

let simplify_stru : vb list -> vb list =
  fun stru -> Stdppx.List.fold_left_map ~f:simplify_vb ~init:Arity_map.empty stru |> snd
;;

let reset_gensym, gensym =
  let n = ref 0 in
  ( (fun () -> n := 0)
  , fun () ->
      incr n;
      !n )
;;

let gensym_s : _ =
  fun ?(prefix = "temp") () ->
  let n = gensym () in
  Printf.sprintf "%s%d" prefix n
;;

let gensym_id ?(prefix = "temp") () = Ident.of_string (gensym_s ~prefix ())

let anf_pat pat ?(kbefore = fun _ -> Fun.id) k =
  let access n e = CApp (APrimitive ("field", 2), AConst (PConst_int n), [ e ]) in
  (* TODO: the use of continuation here is weird, revisit it later. *)
  let rec helper pat ident_name k =
    match pat with
    | Typedtree.Tpat_var s -> make_let_nonrec s (cvar ident_name) @@ k ()
    | Tpat_tuple (a, b, xs) ->
      let rec loop i ps =
        match ps with
        | [] -> k ()
        | Typedtree.Tpat_var name_a :: tl ->
          make_let_nonrec name_a (access i (AVar ident_name)) (loop (1 + i) tl)
        | h :: tl ->
          let name_a = Ident.of_string (gensym_s ()) in
          make_let_nonrec
            name_a
            (access i (AVar ident_name))
            (helper h name_a (fun _ -> loop (1 + i) tl))
      in
      loop 0 (a :: b :: xs)
    | _ -> failwith "not implemented"
  in
  match pat with
  | Typedtree.Tpat_var s -> kbefore s (k s)
  | Tpat_tuple _ ->
    let name_p = Ident.of_string (gensym_s ()) in
    kbefore name_p (helper pat name_p (fun () -> k name_p))
  | _ -> failwith "not implemented"
;;

let anf =
  (* Standard pitfall: forgot to call continuation *)
  let rec helper e (k : imm_expr -> expr) =
    match e with
    | Typedtree.TConst n -> k @@ AConst n
    | TApp (TApp (TVar (varname, _, Builtin (bname, 2), _), arg1, _), arg2, _)
      when is_infix_binop varname ->
      helper arg1 (fun arg1 ->
        helper arg2 (fun arg2 ->
          let name = gensym_id () in
          ELet
            ( NonRecursive
            , Tpat_var name
            , CApp (APrimitive (bname, 2), arg1, [ arg2 ])
            , k (AVar name) )))
    (* | TApp (TApp (TVar ("fresh", _), arg1, _), arg2, _) ->
       helper arg1 (fun arg1 ->
       helper arg2 (fun arg2 ->
       let name = gensym_id () in
       ELet
       ( NonRecursive
       , Tpat_var name
       , CApp (AVar "fresh", arg1, [ arg2 ])
       , k (AVar name) ))) *)
    | TApp (f, arg1, _) ->
      helper f (fun f ->
        helper arg1 (fun arg1 ->
          let name = gensym_id () in
          ELet (NonRecursive, Tpat_var name, CApp (f, arg1, []), k (AVar name))))
    | TLam (pat, body, _) ->
      anf_pat pat ~kbefore:(fun name e -> elam name e) (fun _pat -> helper body k)
    (* | TLam (PVar pat, body, _) ->
       let name = gensym_s () in
       let body = helper body complex_of_atom in
       make_let_nonrec name (CAtom (ALam (APname pat, body))) (k (AVar name)) *)
    | TLet (flag, name, _typ, TLam (Tpat_var vname, body, _), wher) ->
      ELet
        ( flag
        , name
        , (let name = gensym_id () in
           CAtom
             (ALam
                ( APname vname
                , helper body (fun imm ->
                    ELet
                      (NonRecursive, Tpat_var name, CAtom imm, complex_of_atom (AVar name)))
                )))
        , helper wher complex_of_atom )
    | TLet (_, (Tpat_tuple _ as pat), _typ, rhs, wher) ->
      helper rhs (fun imm_rhs ->
        anf_pat
          ~kbefore:(fun name -> make_let_nonrec name (CAtom imm_rhs))
          pat
          (fun _ -> helper wher k))
    | TLet (flag, name, _typ, rhs, wher) ->
      (* NOTE: CPS in this part is tricky *)
      (* TODO: should we merge this case to the upper one? *)
      helper rhs (fun imm_rhs -> ELet (flag, name, CAtom imm_rhs, helper wher k))
    | TIf (econd, eth, el, _) ->
      helper econd (fun eimm ->
        let name = gensym_id () in
        make_let_nonrec
          name
          (CIte (CAtom eimm, helper eth complex_of_atom, helper el complex_of_atom))
          (k (AVar name)))
    | TVar ("=", _id, _, _) ->
      (* TODO: Could be a bug. Check id too. *)
      k (APrimitive ("=", 2))
    | TVar (_, name, User, _) -> k (AVar name)
    | TVar (_, _, Builtin (_name, _arity), _) -> k (APrimitive (_name, _arity))
    | TUnit -> k AUnit
    | TTuple (ea, eb, [], _) ->
      helper ea (fun aimm ->
        helper eb (fun bimm ->
          let name = gensym_id () in
          make_let_nonrec name (CAtom (ATuple (aimm, bimm, []))) (k (AVar name))))
    | TArray (xs, _) ->
      let name = gensym_id () in
      let rec helper_fold xs ys =
        match xs with
        | h :: tl -> helper h (fun x -> helper_fold tl (x :: ys))
        | [] -> make_let_nonrec name (CAtom (AArray ys)) (k (AVar name))
      in
      helper_fold xs []
    | TTuple (_, _, _ :: _, _) as m ->
      Format.eprintf "%a\n%!" Typedtree.pp_expr m;
      Format.kasprintf
        failwith
        "Not implemented (%s %d); '%a'"
        __FUNCTION__
        __LINE__
        Pprinttyped.pp_hum
        m
    | TConstruct (ident, [], _) ->
      let name = gensym_id () in
      let rhs = CAtom (AConstruct (ident.id, [])) in
      make_let_nonrec name rhs (k (AVar name))
    | TConstruct (ident, args, _) ->
      let rec aux = function
        | hd :: tl, acc -> helper hd (fun x -> aux (tl, x :: acc))
        | [], acc ->
          let name = gensym_id () in
          let rhs = CAtom (AConstruct (ident.id, List.rev acc)) in
          make_let_nonrec name rhs (k (AVar name))
      in
      aux (args, [])
    (* converts TMatch into if-then-else *)
    | TMatch (scrutinee, (case1, cases), _) ->
      let prim_field = APrimitive ("get_arg", 2) in
      let prim_tag = APrimitive ("get_tag", 2) in
      let match_failure = APrimitive ("match_failure", 0) in
      let access n x = CApp (prim_field, AConst (PConst_int n), [ x ]) in
      let cmp a b = CApp (APrimitive ("=", 2), a, [ b ]) in
      let rec process_cases scrut cases k =
        match cases with
        | [] -> k match_failure
        | (pattern, expr) :: rest ->
          let success = helper expr k in
          let failure = process_cases scrut rest k in
          match_pattern pattern scrut success failure
      and match_pattern pat scrut_var success failure =
        let make_ite cond success =
          EComplex (CIte (CAtom (AVar cond), success, failure))
        in
        let compare_with_constant constant =
          let fresh = gensym_id () in
          let rhs = cmp scrut_var constant in
          make_let_nonrec fresh rhs @@ make_ite fresh success
        in
        let compare_tag (ident : Ident.t) success =
          let get_tag = CApp (prim_tag, scrut_var, []) in
          let expected_tag = AConst (PConst_int ident.id) in
          let fresh_for_tag = gensym_id () in
          let compare_tags = cmp (AVar fresh_for_tag) expected_tag in
          let fresh_for_cmp = gensym_id () in
          make_let_nonrec fresh_for_tag get_tag
          @@ make_let_nonrec fresh_for_cmp compare_tags
          @@ make_ite fresh_for_cmp success
        in
        let match_many pats =
          let rec aux i = function
            | [] -> success
            | pat :: tl ->
              let fresh = gensym_id () in
              let success = aux (i + 1) tl in
              let scrut = access i scrut_var in
              make_let_nonrec fresh scrut
              @@ match_pattern pat (AVar fresh) success failure
          in
          aux 0 pats
        in
        match pat with
        | Typedtree.Tpat_any -> success
        | Tpat_var var -> make_let_nonrec var (CAtom scrut_var) success
        | Tpat_const c -> compare_with_constant (AConst c)
        | Tpat_unit -> compare_with_constant (AConst (PConst_int 0))
        | Tpat_constr (ident, []) -> compare_tag ident success (* TODO? : delete it *)
        | Tpat_constr (ident, args) -> compare_tag ident @@ match_many args
        | Tpat_tuple (p1, p2, ps) -> match_many (p1 :: p2 :: ps)
      in
      let k scrut =
        let fresh = gensym_id () in
        let wher = process_cases (AVar fresh) (case1 :: cases) k in
        make_let_nonrec fresh (CAtom scrut) wher
      in
      helper scrutinee k
  in
  fun e -> helper e complex_of_atom
;;

let anf_vb vb : vb =
  let anf_body = anf vb.Typedtree.tvb_body in
  let name =
    match vb.tvb_pat with
    | Tpat_var s -> s
    | Tpat_tuple _ -> assert false
    | _ -> failwith "not implemented"
  in
  vb.tvb_flag, name, anf_body
;;

(* TODO : handle type_declarations here *)
let anf_stru stru =
  let open Typedtree in
  let rec aux items acc =
    match items with
    | [] -> List.rev acc
    | Tstr_type _ :: xs -> aux xs acc
    | Tstr_value vb :: xs -> aux xs (anf_vb vb :: acc)
  in
  aux stru []
;;
