(* https://www.cs.swarthmore.edu/~jpolitz/cs75/s16/n_anf-tutorial.html *)
open Miniml

type imm_expr =
  | AConst of Parsetree.const
  | AVar of string
  | APrimitive
  | ALam of string * expr

and c_expr =
  | CApp of imm_expr * imm_expr * imm_expr list
  | CIte of imm_expr * expr * expr

and expr =
  | ELet of Parsetree.rec_flag * string * c_expr * expr
  | EAtom of imm_expr
  | EComplex of c_expr

let pp =
  let open Format in
  let helper ppf e = fprintf ppf "?" in
  helper
;;

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

let anf =
  let rec helper e (k : imm_expr -> expr) =
    match e with
    | Typedtree.TConst n -> k @@ AConst n
    | TApp (f, arg1, _) ->
      helper f (fun f ->
        helper arg1 (fun arg1 ->
          let name = gensym_s () in
          ELet (NonRecursive, name, CApp (f, arg1, []), k (AVar name))))
      (* | TLam (name, bodu, _) -> 
       let name = gensym_s () in
       ELet (NonRecursive, name, CApp (f, arg1, []), k (AVar name)) *)
    | m ->
      Format.kasprintf
        failwith
        "Not implemented (%s %d); '%a'"
        __FUNCTION__
        __LINE__
        Pprinttyped.pp_hum
        m
  in
  fun e -> helper e (fun imm -> EAtom imm)
;;

let test_anf text =
  let ( let* ) x f = Result.bind x f in
  match
    let stru = Miniml.Parsing.parse_vb_exn text in
    (* Format.printf "%s %d\n%!" __FILE__ __LINE__; *)
    let* { tvb_body = typed; _ } = Inferencer.vb stru in
    (* Format.printf "%s %d\n%!" __FILE__ __LINE__; *)
    Result.ok (anf typed)
  with
  | Result.Error err -> Format.printf "%a\n%!" Inferencer.pp_error err
  | Ok anf -> Format.printf "%a\n%!" pp anf
;;

let%expect_test "CPS factorial" =
  test_anf
    {| let rec fack n k =
    if n=0 then k 1
    else fack (n-1) (fun p -> k (p*n)) |};
  [%expect {|  |}]
;;

let%expect_test _ =
  test_anf {| let double a = ((let a = 1 in a), a) |};
  [%expect {|
    (fun a -> let a = 1 in
              (a, a)) |}]
;;
