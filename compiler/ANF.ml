(* https://www.cs.swarthmore.edu/~jpolitz/cs75/s16/n_anf-tutorial.html *)

let log_enabled = ref false
let set_logging b = log_enabled := b

let log fmt =
  if !log_enabled
  then Format.kasprintf (Format.printf "%s\n%!") fmt
  else Format.ifprintf Format.std_formatter fmt
;;

open Miniml

type apat = APname of string

type imm_expr =
  | AUnit
  | AConst of Parsetree.const
  | AVar of string
  | APrimitive of string
  | ATuple of imm_expr * imm_expr * imm_expr list
  | ALam of apat * expr

and c_expr =
  | CApp of imm_expr * imm_expr * imm_expr list
  | CIte of imm_expr * expr * expr
  | CAtom of imm_expr

and expr =
  | ELet of Parsetree.rec_flag * Parsetree.pattern * c_expr * expr
  (* Maybe recursive flag is not required? *)
  | EComplex of c_expr

type vb = Parsetree.rec_flag * string * expr
(* TODO: only complex expression should be there *)

let complex_of_atom x = EComplex (CAtom x)
let make_let_nonrec name rhs wher = ELet (NonRecursive, Parsetree.PVar name, rhs, wher)
let cvar name = CAtom (AVar name)
let alam name e = ALam (APname name, e)
let elam name e = complex_of_atom (alam name e)

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
    | APname s -> pp_print_string ppf s
  ;;

  let rec helper ppf = function
    | ELet (flg, name, CAtom (ALam (arg1, rhs)), wher) ->
      fprintf
        ppf
        "@[<v 2>@[<hov 2>@[let %a%a %a =@]@ "
        Pprint.pp_flg
        flg
        Pprint.pp_pattern
        name
        pp_apat
        arg1;
      fprintf ppf "@[%a@]@ in@]@ @[%a@]@]" helper rhs helper wher
    | ELet (_, name, rhs, wher) ->
      fprintf
        ppf
        "@[<v 2>@[let %a = %a in@]@ @[%a@]@]"
        Pprint.pp_pattern
        name
        helper_c
        rhs
        helper
        wher
    | EComplex (CAtom (AConst c)) -> Pprint.pp_const ppf c
    | EComplex ea -> helper_c ppf ea

  and helper_c ppf = function
    | CApp (APrimitive binop, arg1, [ arg2 ]) when is_infix_binop binop ->
      fprintf ppf "(%a %s %a)" helper_a arg1 binop helper_a arg2
    | CApp (f, arg1, args) ->
      fprintf ppf "@[%a %a %a@]" helper_a f helper_a arg1 (pp_print_list helper_a) args
    | CAtom a -> helper_a ppf a
    | CIte (acond, th, el) ->
      fprintf
        ppf
        "@[<v>@[(if %a@]@ @[then %a@]@ @[else %a)@]@]"
        helper_a
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
    | APrimitive s | AVar s -> fprintf ppf "%s" s
    | ATuple (a, b, ts) -> fprintf ppf "@[(%a)@]" (pp_comma_list helper_a) (a :: b :: ts)
    | AUnit -> fprintf ppf "()"
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
    fprintf ppf "@[<v 2>@[let %a%s " Pprint.pp_flg flg name;
    List.iter (fprintf ppf "%a " pp_apat) pats;
    fprintf ppf "=@]@ @[%a@]@]" pp body
  ;;

  let pp_stru ppf xs = fprintf ppf "@[<v>%a@]" (pp_print_list pp_vb) xs
end

let simplify : expr -> expr =
  let rec helper_a = function
    | ALam (name, e) -> ALam (name, helper e)
    | x -> x
  and helper_c e =
    let rez =
      match e with
      | CAtom a -> CAtom (helper_a a)
      | CApp (f, arg1, args) -> CApp (helper_a f, helper_a arg1, List.map helper_a args)
      | CIte (cond, th, el) -> CIte (helper_a cond, helper th, helper el)
    in
    (* log "Simpl_c: @[%a@] ~~> @[%a@] " pp_c e pp_c rez; *)
    rez
  and helper e =
    let rez =
      match e with
      | EComplex e -> EComplex (helper_c e)
      | ELet (Parsetree.NonRecursive, PVar name1, body, EComplex (CAtom (AVar name2)))
        when String.equal name1 name2 -> EComplex (helper_c body)
      | ELet
          ( NonRecursive
          , PVar name1
          , body
          , ELet (NonRecursive, var2, CAtom (AVar name2), wher_) )
        when String.equal name1 name2 -> helper (ELet (NonRecursive, var2, body, wher_))
      | ELet (flg, name, body, wher) -> ELet (flg, name, helper_c body, helper wher)
    in
    (* log "Simpl: @[%a@] ~~> @[%a@] " pp e pp rez; *)
    rez
  in
  fun e ->
    (* log "Simplification of @[%a@]" pp e; *)
    helper e
;;

let%expect_test _ =
  let ex1 =
    make_let_nonrec
      "temp1"
      (CAtom (alam "f" (complex_of_atom (alam "x" (complex_of_atom (AVar "x"))))))
      (complex_of_atom (AVar "temp1"))
  in
  Format.printf "%a\n~~>\n%a\n%!" pp ex1 pp (simplify ex1);
  [%expect {|
    let temp1 f = (fun x -> x) in
      temp1
    ~~>
    (fun f x -> x) |}];
  let ex1 =
    make_let_nonrec
      "temp1"
      (CAtom
         (alam
            "f"
            (make_let_nonrec
               "temp2"
               (CAtom (alam "x" (complex_of_atom (AVar "x"))))
               (complex_of_atom (AVar "temp2")))))
      (complex_of_atom (AVar "temp1"))
  in
  Format.printf "%a\n~~>\n%a\n%!" pp ex1 pp (simplify ex1);
  [%expect
    {|
    let temp1 f = let temp2 x = x in
                    temp2 in
      temp1
    ~~>
    (fun f x -> x) |}]
;;

let simplify_vb (flag, name, body) = flag, name, simplify body
let simplify_stru eta = List.map simplify_vb eta

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

let anf_pat pat ?(kbefore = fun _ -> Fun.id) k =
  let access n e = CApp (APrimitive "field", AConst (PConst_int n), [ e ]) in
  (* TODO: the use of continuation here is weird, revisit it later. *)
  let rec helper pat ident_name k =
    match pat with
    | Parsetree.PVar s -> make_let_nonrec s (cvar ident_name) @@ k ()
    | PTuple (a, b, xs) ->
      let rec loop i ps =
        match ps with
        | [] -> k ()
        | Parsetree.PVar name_a :: tl ->
          make_let_nonrec name_a (access i (AVar ident_name)) (loop (1 + i) tl)
        | h :: tl ->
          let name_a = gensym_s () in
          make_let_nonrec
            name_a
            (access i (AVar ident_name))
            (helper h name_a (fun _ -> loop (1 + i) tl))
      in
      loop 0 (a :: b :: xs)
  in
  match pat with
  | Parsetree.PVar s -> kbefore s (k s)
  | PTuple _ ->
    let name_p = gensym_s () in
    kbefore name_p (helper pat name_p (fun () -> k name_p))
;;

let test_anf_pat text =
  reset_gensym ();
  match
    let pat = Miniml.Parsing.parse_pat_exn text in
    anf_pat ~kbefore:elam pat (fun _name ->
      complex_of_atom (AVar "use_pattern_vars_here"))
    |> Result.ok
  with
  | Result.Error err -> Format.printf "%a\n%!" Inferencer.pp_error err
  | Ok e -> Format.printf "@[<v>%a@]\n%!" pp e
;;

let%expect_test _ =
  test_anf_pat "x";
  [%expect {| (fun x -> use_pattern_vars_here) |}]
;;

let%expect_test _ =
  test_anf_pat "(x,y)";
  [%expect
    {|
    (fun temp1 -> let x = field 0 temp1 in
                    let y = field 1 temp1 in
                      use_pattern_vars_here) |}]
;;

let%expect_test _ =
  test_anf_pat "(x,y,z)";
  [%expect
    {|
    (fun temp1 -> let x = field 0 temp1 in
                    let y = field 1 temp1 in
                      let z = field 2 temp1 in
                        use_pattern_vars_here) |}]
;;

let%expect_test _ =
  test_anf_pat "((x,y),z)";
  [%expect
    {|
    (fun temp1 -> let temp2 = field 0 temp1 in
                    let x = field 0 temp2 in
                      let y = field 1 temp2 in
                        let z = field 1 temp1 in
                          use_pattern_vars_here) |}]
;;

let anf =
  (* Standard pitfall: forgot to call continuation *)
  let rec helper e (k : imm_expr -> expr) =
    match e with
    | Typedtree.TConst n -> k @@ AConst n
    | TApp (TApp (TVar (varname, _), arg1, _), arg2, _) when is_infix_binop varname ->
      helper arg1 (fun arg1 ->
        helper arg2 (fun arg2 ->
          let name = gensym_s () in
          ELet
            ( NonRecursive
            , PVar name
            , CApp (APrimitive varname, arg1, [ arg2 ])
            , k (AVar name) )))
    | TApp (TApp (TVar ("fresh", _), arg1, _), arg2, _) ->
      helper arg1 (fun arg1 ->
        helper arg2 (fun arg2 ->
          let name = gensym_s () in
          ELet
            (NonRecursive, PVar name, CApp (AVar "fresh", arg1, [ arg2 ]), k (AVar name))))
    | TApp (f, arg1, _) ->
      helper f (fun f ->
        helper arg1 (fun arg1 ->
          let name = gensym_s () in
          ELet (NonRecursive, PVar name, CApp (f, arg1, []), k (AVar name))))
    | TLam (pat, body, _) ->
      anf_pat pat ~kbefore:(fun name e -> elam name e) (fun _pat -> helper body k)
    (* | TLam (PVar pat, body, _) ->
       let name = gensym_s () in
       let body = helper body complex_of_atom in
       make_let_nonrec name (CAtom (ALam (APname pat, body))) (k (AVar name)) *)
    | TLet (flag, name, _typ, TLam (PVar vname, body, _), wher) ->
      ELet
        ( flag
        , name
        , (let name = gensym_s () in
           CAtom
             (ALam
                ( APname vname
                , helper body (fun imm ->
                    ELet (NonRecursive, PVar name, CAtom imm, complex_of_atom (AVar name)))
                )))
        , helper wher complex_of_atom )
    | TLet (_, (PTuple _ as pat), _typ, rhs, wher) ->
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
        let name = gensym_s () in
        make_let_nonrec
          name
          (CIte (eimm, helper eth complex_of_atom, helper el complex_of_atom))
          (k (AVar name)))
    | TVar ("=", _) -> k (APrimitive "=")
    | TVar (name, _) -> k (AVar name)
    | TUnit -> k AUnit
    | TTuple (ea, eb, [], _) ->
      helper ea (fun aimm ->
        helper eb (fun bimm ->
          let name = gensym_s () in
          make_let_nonrec name (CAtom (ATuple (aimm, bimm, []))) (k (AVar name))))
    | TTuple (_, _, _ :: _, _) as m ->
      Format.eprintf "%a\n%!" Typedtree.pp_expr m;
      Format.kasprintf
        failwith
        "Not implemented (%s %d); '%a'"
        __FUNCTION__
        __LINE__
        Pprinttyped.pp_hum
        m
  in
  fun e -> helper e complex_of_atom
;;

let anf_vb vb : vb =
  let anf_body = anf vb.Typedtree.tvb_body in
  let name =
    match vb.tvb_pat with
    | Parsetree.PVar s -> s
    | PTuple _ -> assert false
  in
  vb.tvb_flag, name, anf_body
;;

let anf_stru = List.map anf_vb

let test_anf text =
  reset_gensym ();
  let ( let* ) x f = Result.bind x f in
  match
    let stru = Miniml.Parsing.parse_vb_exn text in
    let vbs = CConv.structure [ stru ] in
    let* vbs_typed = Inferencer.structure vbs in
    (* Format.printf "%s %d\n%!" __FILE__ __LINE__; *)
    anf_stru vbs_typed |> List.map simplify_vb |> Result.ok
  with
  | Result.Error err -> Format.printf "%a\n%!" Inferencer.pp_error err
  | Ok anf -> Format.printf "@[<v>%a@]\n%!" (Format.pp_print_list pp_vb) anf
;;

let%expect_test "CPS factorial" =
  test_anf
    {| let rec fack n k =
    if n=0 then k 1
    else fack (n-1) (fun p -> k (p*n)) |};
  [%expect
    {|
    let fresh_2 n k p =
      let temp1 = (p * n) in
        k temp1
    let rec fack n k =
      let temp3 = (n = 0) in
        (if temp3
        then k 1
        else let temp5 = (n - 1) in
               let temp6 = fack temp5  in
                 let temp7 = fresh_2 n  in
                   let temp8 = temp7 k  in
                     temp6 temp8 ) |}]
;;

let%expect_test _ =
  test_anf {| let double = ((let b = 1 in b), 2) |};
  [%expect {|
    let double =
      let b = 1 in
        (b, 2) |}]
;;

let%expect_test _ =
  test_anf {| let foo = ((fun x -> x), (fun y -> y)) |};
  [%expect
    {|
    let fresh_3 x =
      x
    let fresh_4 y =
      y
    let foo =
      (fresh_3, fresh_4)
     |}]
;;
