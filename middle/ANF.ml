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

type imm_expr =
  | AUnit
  | AConst of Parsetree.const
  | AVar of Ident.t
  | APrimitive of string * int
  | AArray of imm_expr list
  | ALam of apat * expr
[@@deriving show { with_path = false }]

(* TODO(Kakadu): array, lambda, constructor and tuple are not immediates *)
and c_expr =
  | CApp of imm_expr * imm_expr * imm_expr list
  | CIte of c_expr * expr * expr
  | CConstruct of int * imm_expr list
  | CTuple of imm_expr * imm_expr * imm_expr list
  | CAtom of imm_expr
[@@deriving show { with_path = false }]

and expr =
  | ELet of Parsetree.rec_flag * apat * c_expr * expr
  | EComplex of c_expr

and apat =
  | Apat_any
  | Apat_unit
  | Apat_var of Ident.t
  | Apat_const of Parsetree.const

and vb = Parsetree.rec_flag * apat * expr

type stru_item = ANF_vb of vb
type stru = stru_item list

(* TODO: only complex expression should be there *)

let complex_of_atom x = EComplex (CAtom x)
let ecomplex x = EComplex x
let make_let_nonrec name rhs wher = ELet (NonRecursive, Apat_var name, rhs, wher)
let catom i = CAtom i
let cvar name = CAtom (AVar name)
let cite cond th el = CIte (cond, th, el)
let alam name e = ALam (Apat_var name, e)
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
  | "=" | "+" | "-" | "*" | "/" | "<" | "<=" | ">" | ">=" | "&&" | "||" -> true
  | _ -> false
;;

let pp_comma_list eta =
  Format.pp_print_list ~pp_sep:(fun ppf () -> Format.fprintf ppf ", ") eta
;;

(** Formatting *)
include struct
  open Format

  let pp_apat ppf = function
    | Apat_var s -> Ident.pp ppf s
    | Apat_any -> fprintf ppf "_"
    | Apat_unit -> fprintf ppf "()"
    | Apat_const c -> Pprint.pp_const ppf c
  ;;

  let is_simple_rhs = function
    | CConstruct _ | CAtom _ | CApp _ -> true
    | _ -> false
  ;;

  let rec helper ppf = function
    | ELet (flg, patt, CAtom (ALam (arg1, rhs)), wher) ->
      fprintf
        ppf
        "@[<v 2>@[<hov 2>@[let %a%a %a =@]@ "
        Pprint.pp_flg
        flg
        helper_p
        patt
        pp_apat
        arg1;
      fprintf ppf "@[%a@]@ in@]@ @[%a@]@]" helper rhs helper wher
    | ELet (_, patt, rhs, wher) when is_simple_rhs rhs ->
      fprintf ppf "@[<v>";
      fprintf ppf "@[let %a = %a in@]@ " helper_p patt helper_c rhs;
      fprintf ppf "@[%a@]" helper wher;
      fprintf ppf "@]"
    | ELet (_, patt, rhs, wher) ->
      fprintf
        ppf
        "@[<v 2>@[let %a = %a in@]@ @[%a@]@]"
        helper_p
        patt
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
    | CTuple (a, b, ts) -> fprintf ppf "@[(%a)@]" (pp_comma_list helper_a) (a :: b :: ts)
    | CConstruct (id, []) -> fprintf ppf "Constr_%d" id
    | CConstruct (id, [ arg ]) -> fprintf ppf "@[(Constr_%d %a)@]" id helper_a arg
    | CConstruct (id, args) ->
      fprintf ppf "@[(Constr_%d (%a))@]" id (pp_comma_list helper_a) args
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
    | AArray xs -> fprintf ppf "@[[|%a|]@]" (pp_comma_list helper_a) xs
    | AUnit -> fprintf ppf "()"

  and helper_p ppf = function
    | Apat_any -> fprintf ppf "_"
    | Apat_unit -> fprintf ppf "()"
    | Apat_var name -> fprintf ppf "%a" Ident.pp name
    | Apat_const const -> fprintf ppf "%a" Pprint.pp_const const
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

  let pp_stru_item ppf = function
    | ANF_vb (flg, name, expr) ->
      let pats, body = group_abstractions expr in
      fprintf ppf "@[<v 2>@[let %a%a " Pprint.pp_flg flg helper_p name;
      List.iter (fprintf ppf "%a " pp_apat) pats;
      fprintf ppf "=@]@ @[%a@]@]" pp body
  ;;

  let pp_stru ppf (items : stru) =
    fprintf ppf "@[<v>%a@]" (pp_print_list pp_stru_item) items
  ;;
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
    | CTuple (a, b, cs) ->
      helper_i a;
      helper_i b;
      List.iter helper_i cs
    | CConstruct (_, args) -> List.iter helper_i args
    | CAtom i -> helper_i i
  and helper_i = function
    | AVar id when Ident.equal id name -> incr used
    | APrimitive _ | AUnit | AConst _ | AVar _ -> ()
    | ALam (_, e) -> helper e
    | AArray xs -> List.iter helper_i xs
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
    | CTuple (a, b, cs) ->
      helper_i a;
      helper_i b;
      List.iter helper_i cs
    | CConstruct (_, args) -> List.iter helper_i args
    | CAtom (AVar id) when Ident.equal id name -> incr used
    | CAtom i -> helper_i i
  and helper_i = function
    | AVar id when Ident.equal id name -> incr used
    | APrimitive _ | AUnit | AConst _ | AVar _ -> ()
    | ALam (_, e) -> helper e
    | AArray xs -> List.iter helper_i xs
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
  let rec helper x =
    (* log " SubstituteE %a ~~> %a" Ident.pp ident1 pp_c rhs;
    log " inside @[%a@]" pp x; *)
    match x with
    | EComplex c -> ecomplex (helper_c c)
    | ELet (flg, pat, cexpr, expr) ->
      let new_rhs = helper_c cexpr in
      elet flg pat new_rhs (helper expr)
  and helper_c ce =
    (* log " SubstituteC %a ~~> %a" Ident.pp ident1 pp_c rhs;
    log " inside @[%a@]" pp_c ce; *)
    match ce with
    | CAtom (AVar x) when Ident.equal x ident1 -> rhs
    | CAtom (AVar _) as c -> c
    | CAtom i -> catom (helperi i)
    | CIte (CAtom (AVar name), ethen, eelse) when Ident.equal ident1 name ->
      cite rhs (helper ethen) (helper eelse)
    | CIte (cond, ethen, eelse) -> cite (helper_c cond) (helper ethen) (helper eelse)
    | CApp (AVar x, arg1, args) when Ident.equal x ident1 ->
      (match rhs with
       | CApp (f, arg0, arg_mid) -> CApp (f, arg0, arg_mid @ (arg1 :: args))
       | CAtom (AVar _ as new_) -> CApp (new_, arg1, args)
       | _ -> assert false)
    | CApp ((APrimitive _ as f), _arg1, _args) ->
      CApp (f, helperi _arg1, List.map helperi _args)
    | CApp ((AVar _ as _f), _arg1, _args) ->
      let map = function
        | AVar v when Ident.equal ident1 v ->
          (match rhs with
           | CAtom rhs -> rhs
           | _ ->
             Format.eprintf "rhs = %a\n" pp_c rhs;
             failwiths "Substitution implemented badly")
        | x -> x
      in
      CApp (_f, map _arg1, List.map map _args)
    | CTuple (i1, i2, is) -> CTuple (helperi i1, helperi i2, List.map helperi is)
    | CConstruct (tag, is) -> CConstruct (tag, List.map helperi is)
    | c ->
      Format.eprintf "%a\n%!" pp_c c;
      Format.eprintf "can't substitute %a -> %a\n%!" Ident.pp ident1 pp_c rhs;
      assert false
  and helperi x =
    log "  SubstituteI %a ~~> %a" Ident.pp ident1 pp_c rhs;
    log "  inside @[%a@]" pp_a x;
    match x with
    | AConst (PConst_bool true) -> AConst (PConst_int 1)
    | AConst (PConst_bool false) -> AConst (PConst_int 0)
    | (APrimitive _ | AConst _ | AUnit) as i -> i
    | AArray is -> AArray (List.map helperi is)
    | AVar name when Ident.equal ident1 name ->
      (match rhs with
       | CAtom a -> a
       | _ ->
         Format.eprintf "Possible missing substitution. %s %d\n%!" __FILE__ __LINE__;
         let _ = failwith "not implemented" in
         AVar name)
    | AVar _ as i -> i
    | i ->
      Format.eprintf "%a\n%!" pp_a i;
      failwiths "%s: unsupported case" __FUNCTION__
  in
  (* log "Substitute %a ~~> %a START" Ident.pp ident1 pp_c rhs; *)
  let ans = helper where in
  (* log "Substitute %a ~~> %a" Ident.pp ident1 pp_c rhs;
  log "inside @[%a@]" pp where;
  log "gives @[%a@] FIN" pp ans; *)
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
            (Apat_var v7)
            (CAtom (AVar vf))
            (EComplex (CApp (AVar vx, AVar v9, []))));
  [%expect
    {|
    let v7 = vf in
    vf 1 v9
    |}]
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
  let is_comparison : c_expr -> bool =
    let arg_is_imm = function
      | AConst (PConst_int _) | AConst (PConst_char _) | AConst (PConst_bool _) -> true
      | _ -> false
    in
    function
    (* There we want to detect integer comparison *)
    | CApp (APrimitive (("<" | "=" | "<="), _), l, [ r ]) ->
      (* TODO(Kakadu): fix here, when we get user-defined operators *)
      arg_is_imm l || arg_is_imm r
    | _ -> false
  in
  let rec helper_a acc = function
    | ALam (name, e) -> ALam (name, helper acc e)
    | x -> x
  and helper_c acc e =
    let rez =
      match e with
      | CAtom a -> CAtom (helper_a acc a)
      | CConstruct (a, args) -> CConstruct (a, args)
      | CTuple (a, b, bs) -> CTuple (a, b, bs)
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
      (* inline for variable application *)
      | ELet
          ( Parsetree.NonRecursive
          , Apat_var name1
          , (CApp (AVar fname, _arg1, args) as rhs)
          , where_ )
        when used_once_as_function name1 ~where:where_
             && Arity_map.is_under fname (1 + List.length args) acc
             && cfg.opt_arity_inline ->
        helper acc (substitute ~where:where_ name1 rhs)
        (* inline for primitive application *)
      | ELet
          ( Parsetree.NonRecursive
          , Apat_var name1
          , (CApp (APrimitive (_fname, parity), _arg1, args) as rhs)
          , where_ )
        when used_once_as_function name1 ~where:where_
             && 1 + List.length args < parity
             && cfg.opt_arity_inline -> helper acc (substitute ~where:where_ name1 rhs)
      | ELet
          ( Parsetree.NonRecursive
          , Apat_var name1
          , (CApp (APrimitive (_fname, parity), _arg1, args) as rhs)
          , where_ )
        when used_once_as_function name1 ~where:where_
             && 1 + List.length args < parity
             && cfg.opt_arity_inline -> helper acc (substitute ~where:where_ name1 rhs)
      | ELet (Parsetree.NonRecursive, Apat_var name1, body, EComplex (CAtom (AVar name2)))
        when Ident.equal name1 name2 ->
        (* let x = x in ... *)
        EComplex (helper_c acc body)
      (* | ELet
          ( NonRecursive
          , Apat_var name1
          , body
          , ELet (NonRecursive, var2, CAtom (AVar name2), wher_) )
        when Ident.equal name1 name2 ->
        (* let name1 = ... in
           let ...  = name1 in *)
        helper acc (ELet (NonRecursive, var2, body, wher_)) *)
      | ELet (NonRecursive, Apat_var v1, rhs, where)
        when used_once_in_if v1 ~where && is_comparison rhs && cfg.opt_cmp_into_if_inline
        -> helper acc (substitute ~where v1 rhs)
      | ELet (NonRecursive, Apat_var v1, (CAtom (AVar _v2) as rhs), where) ->
        helper acc (substitute ~where v1 rhs)
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

let simplify_vb acc (flag, patt, body) =
  let get_arity x =
    match group_abstractions x with
    | [], _ -> 0
    | xs, _ -> List.length xs
  in
  match flag, patt with
  | Parsetree.Recursive, Apat_var name ->
    let arity = get_arity body in
    let new_acc = Arity_map.add name.hum_name arity acc in
    new_acc, (flag, patt, simplify new_acc body)
  | NonRecursive, Apat_var name ->
    let arity = get_arity body in
    let new_acc = Arity_map.add name.hum_name arity acc in
    new_acc, (flag, patt, simplify acc body)
  | _ -> acc, (flag, patt, body)
;;

let simplify_stru_item acc = function
  | ANF_vb vb ->
    let new_acc, new_vb = simplify_vb acc vb in
    new_acc, ANF_vb new_vb
;;

let simplify_stru (stru : stru) : stru =
  Stdppx.List.fold_left_map ~f:simplify_stru_item ~init:Arity_map.empty stru |> snd
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
  let access n e = CApp (APrimitive ("block_nth", 2), e, [ AConst (PConst_int n) ]) in
  let compare_tag ~scrut ~expected k =
    let fresh = gensym_id ~prefix:"tag" () in
    let get_tag k =
      ELet
        ( Parsetree.NonRecursive
        , Apat_var fresh
        , CApp (APrimitive ("block_tag", 1), AVar scrut, [])
        , k )
    in
    get_tag
      (EComplex
         (CIte
            ( CApp (APrimitive ("=", 2), AVar fresh, [ AConst (PConst_int expected) ])
            , k
            , EComplex (CAtom (APrimitive ("match_failure", 0))) )))
  in
  let rec access_fields lhs_patts rhs_ident k =
    let rec loop i = function
      | [] -> k ()
      | Typedtree.Tpat_var name_a :: tl ->
        make_let_nonrec name_a (access i (AVar rhs_ident)) (loop (1 + i) tl)
      | h :: tl ->
        let name_a = Ident.of_string (gensym_s ~prefix:"field" ()) in
        make_let_nonrec
          name_a
          (access i (AVar rhs_ident))
          (helper h name_a (fun _ -> loop (1 + i) tl))
    in
    loop 0 lhs_patts
  and helper pat ident_name k =
    match pat with
    (* TODO: it is not optimal *)
    | Tpat_any -> elet Parsetree.NonRecursive Apat_any (cvar ident_name) @@ k ()
    | Tpat_unit -> elet Parsetree.NonRecursive Apat_unit (cvar ident_name) @@ k ()
    | Tpat_const const ->
      elet Parsetree.NonRecursive (Apat_const const) (cvar ident_name) @@ k ()
    | Typedtree.Tpat_var s -> make_let_nonrec s (cvar ident_name) @@ k ()
    | Tpat_tuple (p1, p2, ps) -> access_fields (p1 :: p2 :: ps) ident_name k
    | Tpat_constr (name, ps) ->
      compare_tag ~scrut:ident_name ~expected:name.id (access_fields ps ident_name k)
  in
  match pat with
  | Typedtree.Tpat_var s -> kbefore s (k s)
  | Tpat_tuple _ ->
    let name_p = Ident.of_string (gensym_s ~prefix:"tuple" ()) in
    kbefore name_p (helper pat name_p (fun () -> k name_p))
  | Tpat_constr _ ->
    let name_p = Ident.of_string (gensym_s ~prefix:"adt" ()) in
    kbefore name_p (helper pat name_p (fun () -> k name_p))
  | _ ->
    let name_p = Ident.of_string (gensym_s ~prefix:"weird" ()) in
    kbefore name_p (helper pat name_p (fun () -> k name_p))
;;

let anf =
  let has_list_typ e =
    let rec helper t =
      match t.Typedtree.typ_desc with
      | TConstr ([ _ ], "list") ->
        log "has_list_typ says true";
        true
      | TLink t -> helper t
      | _ ->
        log "%a" Typedtree.pp_ty t;
        log "has_list_typ says false";
        false
    in
    let t = Typedtree.type_of_expr e in
    helper t
  in
  let get_tag x = CApp (APrimitive ("block_tag", 1), x, []) in
  let access obj n = CApp (APrimitive ("block_nth", 2), obj, [ AConst (PConst_int n) ]) in
  let on_matching helper scrutinee cases (k : imm_expr -> expr) =
    let match_failure =
      CApp (APrimitive ("match_failure", 1), AConst (Parsetree.PConst_int 666), [])
    in
    let cmp a b = CApp (APrimitive ("=", 2), a, [ b ]) in
    let rec process_cases scrut cases k =
      match cases with
      | [] ->
        let fresh = gensym_id () in
        make_let_nonrec fresh match_failure (k (AVar fresh))
      | (pattern, expr) :: rest ->
        let success = helper expr k in
        let failure = process_cases scrut rest k in
        match_pattern pattern scrut success failure
    and match_pattern pat scrut_var success failure =
      let make_ite cond success = EComplex (CIte (CAtom (AVar cond), success, failure)) in
      let compare_with_constant constant =
        let fresh = gensym_id () in
        let rhs = cmp scrut_var constant in
        make_let_nonrec fresh rhs @@ make_ite fresh success
      in
      let compare_tag (ident : Ident.t) success =
        let expected_tag = AConst (PConst_int ident.id) in
        let fresh_for_tag = gensym_id () in
        let compare_tags = cmp (AVar fresh_for_tag) expected_tag in
        let fresh_for_cmp = gensym_id () in
        make_let_nonrec fresh_for_tag (get_tag scrut_var)
        @@ make_let_nonrec fresh_for_cmp compare_tags
        @@ make_ite fresh_for_cmp success
      in
      let match_many pats =
        let rec aux i = function
          | [] -> success
          | pat :: tl ->
            let fresh = gensym_id () in
            let success = aux (i + 1) tl in
            let scrut = access scrut_var i in
            make_let_nonrec fresh scrut @@ match_pattern pat (AVar fresh) success failure
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
      let wher = process_cases (AVar fresh) cases k in
      make_let_nonrec fresh (CAtom scrut) wher
    in
    helper scrutinee k
  in
  (* Standard pitfall: forgot to call continuation *)
  let rec helper e (k : imm_expr -> expr) =
    match e with
    | Typedtree.TConst n -> k @@ AConst n
    | TFormat (s, _ty) -> k @@ AConst (PConst_string s)
    | TApp (TApp (TVar (varname, _, Builtin (bname, 2), _), arg1, _), arg2, _)
      when is_infix_binop varname ->
      helper arg1 (fun arg1 ->
        helper arg2 (fun arg2 ->
          let name = gensym_id () in
          ELet
            ( NonRecursive
            , Apat_var name
            , CApp (APrimitive (bname, 2), arg1, [ arg2 ])
            , k (AVar name) )))
    | TApp (f, arg1, _ty) ->
      helper f (fun f ->
        helper arg1 (fun arg1 ->
          let name = gensym_id () in
          ELet (NonRecursive, Apat_var name, CApp (f, arg1, []), k (AVar name))))
    | TLam (Typedtree.Tpat_unit, body, _) ->
      EComplex (CAtom (ALam (Apat_unit, helper body k)))
    | TLam (pat, body, _) ->
      anf_pat pat ~kbefore:(fun name e -> elam name e) (fun _pat -> helper body k)
    | TLet (flag, Tpat_var name, _typ, TLam (Tpat_var vname, body, _), wher) ->
      ELet
        ( flag
        , Apat_var name
        , CAtom (ALam (Apat_var vname, helper body complex_of_atom))
        , helper wher complex_of_atom )
    | TLet (_, pat, _typ, rhs, wher) ->
      helper rhs (fun imm_rhs ->
        anf_pat
          ~kbefore:(fun name -> make_let_nonrec name (CAtom imm_rhs))
          pat
          (fun _ -> helper wher k))
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
    | TUnit -> k (AConst (PConst_int 0))
    | TTuple (ea, eb, es, _) ->
      helper ea (fun aimm ->
        helper eb (fun bimm ->
          let rec helper_fold es imms =
            match es with
            | hd :: tl -> helper hd (fun imm -> helper_fold tl (imm :: imms))
            | [] ->
              let name = gensym_id () in
              let atuple = CTuple (aimm, bimm, List.rev imms) in
              make_let_nonrec name atuple (k (AVar name))
          in
          helper_fold es []))
    | TArray (xs, _) ->
      let name = gensym_id () in
      let rec helper_fold xs ys =
        match xs with
        | h :: tl -> helper h (fun x -> helper_fold tl (x :: ys))
        | [] -> make_let_nonrec name (CAtom (AArray ys)) (k (AVar name))
      in
      helper_fold xs []
    | TConstruct (ident, [], _) ->
      (* TODO: Simplify 0-arity constructors into numbers *)
      let name = gensym_id () in
      let rhs = CConstruct (ident.id, []) in
      make_let_nonrec name rhs (k (AVar name))
    | TConstruct (ident, args, _) ->
      let rec aux = function
        | hd :: tl, acc -> helper hd (fun x -> aux (tl, x :: acc))
        | [], acc ->
          let name = gensym_id () in
          let rhs = CConstruct (ident.id, List.rev acc) in
          make_let_nonrec name rhs (k (AVar name))
      in
      aux (args, [])
    (* A specialization to detect list matching  *)
    | TMatch (scrutinee, (case1, [ (p2, rhs2) ]), _) when has_list_typ scrutinee ->
      (* log "p2 = %a" Typedtree.pp_pattern p2; *)
      (match fst case1, p2 with
       | Tpat_constr (pi1, []), Tpat_constr (pi2, [ Tpat_var pih; Tpat_var pitl ])
         when pi1.Ident.hum_name = "[]" && pi2.Ident.hum_name = "::" ->
         helper scrutinee (fun scrut ->
           let fresh = gensym_id () in
           make_let_nonrec fresh (CAtom scrut)
           @@
           let fresh_tag = gensym_id () in
           make_let_nonrec fresh_tag (get_tag (AVar fresh))
           @@ EComplex
                (CIte
                   ( CApp
                       ( APrimitive ("=", 2)
                       , AVar fresh_tag
                       , [ AConst (Parsetree.PConst_int 0) ] )
                   , helper (snd case1) k
                   , make_let_nonrec pih (access scrut 0)
                     @@ make_let_nonrec pitl (access scrut 1)
                     @@ helper rhs2 k )))
       | _ -> on_matching helper scrutinee [ case1; p2, rhs2 ] k)
    (* converts TMatch into if-then-else *)
    | TMatch (scrutinee, (case1, cases), _) ->
      (* log "case1 = %a" Typedtree.pp_pattern (fst case1); *)
      on_matching helper scrutinee (case1 :: cases) k
  in
  fun e -> helper e complex_of_atom
;;

(* TODO: cps here is silly *)

let anf_vb_k ?(flg = Parsetree.NonRecursive) apatt body k = ANF_vb (flg, apatt, body) :: k

let anf_stru_item (vb : Typedtree.value_binding) : stru_item list =
  let access obj n =
    EComplex (CApp (APrimitive ("block_nth", 2), obj, [ AConst (PConst_int n) ]))
  in
  let compare_tag ~scrut ~tag =
    anf_vb_k
      (Apat_const (PConst_int tag))
      (EComplex (CApp (APrimitive ("block_tag", 1), AVar scrut, [])))
  in
  let rec anf_vbs_from_tvb ?(flg = Parsetree.NonRecursive) lhs rhs k =
    match (lhs : Typedtree.pattern) with
    | Tpat_any -> anf_vb_k Apat_any rhs k
    | Tpat_unit -> anf_vb_k Apat_unit rhs k
    | Tpat_var name -> anf_vb_k ~flg (Apat_var name) rhs k
    | Tpat_const const -> anf_vb_k (Apat_const const) rhs k
    | Tpat_tuple (p1, p2, ps) ->
      let fresh = gensym_id ~prefix:"tuple" () in
      let k = access_fields (p1 :: p2 :: ps) fresh k in
      anf_vb_k (Apat_var fresh) rhs k
    | Tpat_constr (variant, ps) ->
      let fresh = gensym_id ~prefix:"adt" () in
      let k = compare_tag ~scrut:fresh ~tag:variant.id (access_fields ps fresh k) in
      anf_vb_k (Apat_var fresh) rhs k
  and access_fields lhs_fields rhs_ident k =
    let rec helper lhs_fields n k =
      match lhs_fields with
      | [] -> []
      | head :: tail ->
        let k = helper tail (n + 1) k in
        anf_vbs_from_tvb head (access (AVar rhs_ident) n) k
    in
    helper lhs_fields 0 k
  in
  let rhs = anf vb.Typedtree.tvb_body in
  anf_vbs_from_tvb ~flg:vb.tvb_flag vb.tvb_pat rhs []
;;

let anf_stru stru =
  let open Typedtree in
  let rec aux items acc =
    match items with
    | [] -> List.concat (List.rev acc)
    | Tstr_type _ :: xs -> aux xs acc
    | Tstr_value vb :: xs -> aux xs (anf_stru_item vb :: acc)
  in
  aux stru []
;;

type iterator =
  { aconst : iterator -> Parsetree.const -> unit
  ; avar : iterator -> Ident.t -> unit
  ; aprimitive : iterator -> string -> int -> unit
  ; ctuple : iterator -> imm_expr -> imm_expr -> imm_expr list -> unit
  ; cconstruct : iterator -> int -> imm_expr list -> unit
  ; aarray : iterator -> imm_expr list -> unit
  ; alam : iterator -> apat -> expr -> unit
  ; catom : iterator -> imm_expr -> unit
  ; cite : iterator -> c_expr -> expr -> expr -> unit
  ; capp : iterator -> imm_expr -> imm_expr -> imm_expr list -> unit
  ; elet : iterator -> Parsetree.rec_flag -> apat -> c_expr -> expr -> unit
  ; cconst_string : iterator -> string -> unit
  ; on_expr : iterator -> expr -> unit
  ; on_cexpr : iterator -> c_expr -> unit
  ; on_imm : iterator -> imm_expr -> unit
  }

let default_iterator =
  { avar = (fun _self _ -> ())
  ; aconst = (fun _self _ -> ())
  ; aprimitive = (fun _self _ _ -> ())
  ; ctuple = (fun self a1 a2 ass -> List.iter (self.on_imm self) (a1 :: a2 :: ass))
  ; cconstruct = (fun self _tag es -> List.iter (self.on_imm self) es)
  ; aarray = (fun self es -> List.iter (self.on_imm self) es)
  ; alam = (fun self _ e -> self.on_expr self e)
  ; catom = (fun self x -> self.on_imm self x)
  ; cite =
      (fun self c th el ->
        self.on_cexpr self c;
        self.on_expr self th;
        self.on_expr self el)
  ; capp = (fun self a1 a2 ass -> List.iter (self.on_imm self) (a1 :: a2 :: ass))
  ; cconst_string = (fun _ _ -> ())
  ; elet =
      (fun self _flg _pat cexpr expr ->
        self.on_cexpr self cexpr;
        self.on_expr self expr)
  ; on_expr =
      (fun self -> function
         | EComplex c -> self.on_cexpr self c
         | ELet (flg, _pat, c, e) -> self.elet self flg _pat c e)
  ; on_cexpr =
      (fun self x ->
        match x with
        | CAtom imm -> self.catom self imm
        | CTuple (a, b, cs) -> self.ctuple self a b cs
        | CConstruct (tag, ass) -> self.cconstruct self tag ass
        | CIte (c, th, el) -> self.cite self c th el
        | CApp (f, arg1, args) -> self.capp self f arg1 args)
  ; on_imm =
      (fun self -> function
         | AUnit -> ()
         | AConst c -> self.aconst self c
         | AVar v -> self.avar self v
         | APrimitive (name, arity) -> self.aprimitive self name arity
         | AArray xs -> self.aarray self xs
         | ALam (pat, e) -> self.alam self pat e)
  }
;;
