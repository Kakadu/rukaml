(* https://www.cs.swarthmore.edu/~jpolitz/cs75/s16/n_anf-tutorial.html *)
open Miniml

type imm_expr =
  | AConst of Parsetree.const
  | AVar of string
  | APrimitive of string
  | ATuple of imm_expr * imm_expr * imm_expr list
  | ALam of string * expr

and c_expr =
  | CApp of imm_expr * imm_expr * imm_expr list
  | CIte of imm_expr * expr * expr
  | CAtom of imm_expr

and expr =
  | ELet of Parsetree.rec_flag * string * c_expr * expr
  | EComplex of c_expr

type vb = Parsetree.rec_flag * string * expr

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

  let pp =
    let rec helper ppf = function
      | ELet (flg, name, CAtom (ALam (arg1, rhs)), wher) ->
        fprintf
          ppf
          "@[<v 2>@[<hov 2>@[let %a%s %s =@]@ @[%a@]@ in@]@ @[%a@]@]"
          Pprint.pp_flg
          flg
          name
          arg1
          helper
          rhs
          helper
          wher
      | ELet (_, name, rhs, wher) ->
        fprintf ppf "@[<v 2>@[let %s = %a in@]@ @[%a@]@]" name helper_c rhs helper wher
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
          "@[<v>@[if %a@]@ @[then %a@]@ @[else %a@]@]"
          helper_a
          acond
          helper
          th
          helper
          el
    and helper_a ppf = function
      | ALam (arg1, EComplex (CAtom (ALam (arg2, e)))) ->
        fprintf ppf "@[(fun %s %s -> %a)@]" arg1 arg2 helper e
      | ALam (name, e) -> fprintf ppf "(fun %s -> %a)" name helper e
      | AConst c -> Pprint.pp_const ppf c
      | APrimitive s | AVar s -> fprintf ppf "%s" s
      | ATuple (a, b, ts) -> fprintf ppf "@[(%a)@]" (pp_comma_list helper_a) (a :: b :: ts)
    in
    helper
  ;;

  let pp_vb ppf (flg, name, expr) =
    fprintf ppf "@[<v 2>@[let %a%s =@]@ @[%a@]@]" Pprint.pp_flg flg name pp expr
  ;;
end

let simplify : expr -> expr =
  let rec helper_a = function
    | ALam (name, e) -> ALam (name, helper e)
    | x -> x
  and helper_c = function
    | CAtom a -> CAtom (helper_a a)
    | CApp (f, arg1, args) -> CApp (helper_a f, helper_a arg1, List.map helper_a args)
    | CIte (cond, th, el) -> CIte (helper_a cond, helper th, helper el)
  and helper = function
    | EComplex e -> EComplex (helper_c e)
    | ELet (Parsetree.NonRecursive, name1, body, EComplex (CAtom (AVar name2)))
      when String.equal name1 name2 -> EComplex (helper_c body)
    | ELet (flg, name, body, wher) -> ELet (flg, name, helper_c body, helper wher)
  in
  helper
;;

let simplify_vb (flag, name, body) = flag, name, simplify body

let gensym =
  let n = ref 0 in
  fun () ->
    incr n;
    !n
;;

let gensym_s : _ =
 fun ?(prefix = "temp") () ->
  let n = gensym () in
  Printf.sprintf "%s%d" prefix n
;;

let complex_of_atom x = EComplex (CAtom x)

let anf =
  let rec helper e (k : imm_expr -> expr) =
    match e with
    | Typedtree.TConst n -> k @@ AConst n
    | TApp (TApp (TVar (varname, _), arg1, _), arg2, _) when is_infix_binop varname ->
      helper arg1 (fun arg1 ->
        helper arg2 (fun arg2 ->
          let name = gensym_s () in
          ELet
            (NonRecursive, name, CApp (APrimitive varname, arg1, [ arg2 ]), k (AVar name))))
    | TApp (f, arg1, _) ->
      helper f (fun f ->
        helper arg1 (fun arg1 ->
          let name = gensym_s () in
          ELet (NonRecursive, name, CApp (f, arg1, []), k (AVar name))))
    | TLam (v, body, _) ->
      let name = gensym_s () in
      let body : expr = helper body (fun imm -> EComplex (CAtom imm)) in
      ELet (NonRecursive, name, CAtom (ALam (v, body)), k (AVar name))
    | TLet (flag, name, _typ, TLam (vname, body, _), wher) ->
      ELet
        ( flag
        , name
        , (let name = gensym_s () in
           CAtom
             (ALam
                ( vname
                , helper body (fun imm ->
                    ELet (NonRecursive, name, CAtom imm, complex_of_atom (AVar name))) )))
        , helper wher complex_of_atom )
    | TLet (flag, name, _typ, rhs, wher) ->
      (* NOTE: CPS in this part is triky *)
      helper rhs (fun imm_rhs -> ELet (flag, name, CAtom imm_rhs, helper wher k))
    | TIf (econd, eth, el, _) ->
      helper econd (fun eimm ->
        EComplex (CIte (eimm, helper eth complex_of_atom, helper el complex_of_atom)))
    | TVar ("=", _) -> k @@ APrimitive "="
    | TVar (name, _) -> k (AVar name)
    | TTuple (ea, eb, [], _) ->
      helper ea (fun aimm ->
        helper eb (fun bimm -> complex_of_atom (ATuple (aimm, bimm, []))))
    | m ->
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
    let fresh_2 =
      (fun n k -> (fun p -> let temp4 = (p * n) in
                              k temp4 ))
    let rec fack =
      (fun n k -> let temp8 = (n = 0) in
                    if temp8
                    then k 1
                    else let temp9 = (n - 1) in
                           let temp10 = fack temp9  in
                             let temp11 = fresh_2 n  in
                               let temp12 = temp11 k  in
                                 temp10 temp12 ) |}]
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
    let fresh_3 =
      (fun x -> x)
    let fresh_4 =
      (fun y -> y)
    let foo =
      (fresh_3, fresh_4)
     |}]
;;
