(*
   test
  (targets amd64 rv64)
  (run (stdout
          "(0) parsed var: x"
          "(1) parsed var: x"
          "(2) parsed var: x"
          "(3) parsed expr: x"
          "(4) parsed expr: (x y)"
          "(5) parsed expr: (> x . x y)"
          "(6) parsed expr: (> f . > x . > y . f (x y))"
          "(7) parsed expr: (> f . > x . > y . f (x y))"
          "(8) parsed expr: ((> x . x) a)"
          "(9) parsed expr: ((> f . > x . f x) (> y . y))"
          "(17) parsed expr: ((> x . > y . > z . x (z (y z))) (a (b c)))"
          "(18) parsed expr: ((> k . > x . k) (> x . > y . x))"
          "(27) Omega: ((> x . x x) (> x . x x))"
          "(32) Church 2: (> f . > x . f (f x))"
          "(34) plus: (> m . > n . > f . > x . m (f (n (f x))))"
          "(35) mult: (> m . > n . > f . m (n f))"
          "(37) chain: (a (b (c (d (e f)))))"
          "(38) nested: (> x . (> y . y) x)"))
*)

(* TODO: remove this example before merge *)

(* [begin skip] *)

let string_of_char_list chs = String.of_seq (List.to_seq chs)
let string_len s = String.length s
let string_nth s n = s.[n]
let char_code = Char.code
let printf, fprintf, sprintf = Stdlib.Printf.(printf, fprintf, sprintf)

(* [end skip] *)

let print_endline s =
  let() = output_string stdout s in
  print_newline ()

let print_string s = output_string stdout s
let print_int n = print n

type parsing_error =
  | Perr_eof
  | Perr_match of int
  | Perr_unexpected_char of char

let pp_parsing_error oc err =
  match err with
  | Perr_eof -> print_endline "unexpected eof"
  | Perr_match n ->
    let () = print_string  "match failed: " in
    print_int n
  | Perr_unexpected_char ch ->
      let() = output_string stdout "unexpected char: " in
      output_char stdout ch
;;

type 'a parsing_result =
  | Prez_ok of ('a * char list)
  | Prez_err of parsing_error

type 'a parser = char list -> 'a parsing_result
type 'a fix = Fix of ('a fix -> 'a)

let fix f =
  let rec g x =
    match x with
    | Fix x -> f (fun a -> g (Fix x) a)
  in
  g (Fix g)
;;

let rec skip_ws chs =
  match chs with
  | [] -> Prez_ok ((), [])
  | ch :: chs ->
    (* space is 32 *)
    if char_code ch = 32 then skip_ws chs else Prez_ok ((), ch :: chs)
;;

let parse_var chs =
  match skip_ws chs with
  | Prez_err err -> Prez_err err
  | Prez_ok ((), chs) ->
    (match chs with
     | [] -> Prez_err Perr_eof
     | ch :: chs ->
       (* let () = output_char stdout ch in *)
       (* let () = print_endline "got a char" in *)
       let code = char_code ch in

       if 96 < code && code < 123
       then Prez_ok (ch, chs)
       else Prez_err (Perr_unexpected_char ch))
;;

let skip_char ch chs =
  match skip_ws chs with
  | Prez_err err -> Prez_err err
  | Prez_ok ((), hd :: tl) ->
    if ch = hd then Prez_ok ((), tl) else Prez_err (Perr_unexpected_char hd)
  | _ -> Prez_err Perr_eof
;;

let parens parse chs =
  match skip_char '(' chs with
  | Prez_err err -> Prez_err err
  | Prez_ok ((), chs) ->
    (match parse chs with
     | Prez_err err -> Prez_err err
     | Prez_ok (expr, chs) ->
       (match skip_char ')' chs with
        | Prez_err err -> Prez_err err
        | Prez_ok ((), chs) -> Prez_ok (expr, chs))
     | _ -> Prez_err (Perr_match 2))
;;

let string_to_char_list s =
  let len = string_len s in
  let rec helper n = if n >= len then [] else string_nth s n :: helper (n + 1) in
  helper 0
;;

let test0 () =
  match parse_var (string_to_char_list " x ") with
  | Prez_ok (ch, tl) ->

    printf "(0) parsed var: %c\n" ch
  | Prez_err err ->
    (* printf2 "(0) parsing error: %a"  *)
    pp_parsing_error stdout err
;;

let test1() =
  match parens parse_var (string_to_char_list "(x)") with
  | Prez_ok (ch, tl) ->
    printf "(1) parsed var: %c\n" ch
  | Prez_err err ->
     (* printf2 "(1) parsing error: %a"  *)
  pp_parsing_error stdout err
;;

let test2 ()=
  match parens parse_var (string_to_char_list " ( x ) ") with
  | Prez_ok (ch, tl) ->
    printf "(2) parsed var: %c\n" ch
  | Prez_err err ->
    printf "(2) parsing error: %a\n" pp_parsing_error err
;;

type expr =
  | Var of char
  | Abs of (char * expr)
  | App of (expr * expr)

let rec pp_expr parens oc expr =
  match expr with
  | Var name -> fprintf oc "%c" name
  | Abs (name, e) ->
    fprintf oc (if parens then "(> %c . %a)" else "> %c . %a") name (pp_expr false) e
  | App (e1, e2) ->
    fprintf oc (if parens then "(%a %a)" else "%a %a") (pp_expr true) e1 (pp_expr true) e2
;;

let parse_atom parse_expr =
  fix (fun self chs ->
    match parens parse_expr chs with
    | Prez_ok ok -> Prez_ok ok
    | _ ->
      (match parse_var chs with
       | Prez_err err -> Prez_err err
       | Prez_ok (name, chs) -> Prez_ok (Var name, chs)))
;;

let parse_evar chs =
  match parse_var chs with
  | Prez_err err -> Prez_err err
  | Prez_ok (name, tl) -> Prez_ok (Var name, tl)
;;

let test3 ()=
  match parse_atom parse_evar (string_to_char_list " ( x ) ") with
  | Prez_ok (e, tl) ->
    printf "(3) parsed expr: %a\n" (pp_expr true) e
  | Prez_err err ->
    printf "(3) parsing error: %a\n" pp_parsing_error err

;;

let make_left_prio_app first rest =
  let rec unwrap acc term =
    match term with
    | App (left, right) -> unwrap (left :: acc) right
    | _ -> term :: acc
  in
  let rec wrap terms acc =
    match terms with
    | [] -> acc
    | x :: xs -> wrap xs (App (acc, x))
  in
  wrap (unwrap [] rest) first
;;

let parse_app parse_expr =
  fix (fun self chs ->
    match parse_expr chs with
    | Prez_err err -> Prez_err err
    | Prez_ok (left, chs) ->
      (match skip_ws chs with
       | Prez_err err -> Prez_err err
       | Prez_ok ((), chs) ->
         (match self chs with
          | Prez_ok (right, chs) ->
            (* TODO: replace App (left, right) with make_left_prio_app left right *)
            Prez_ok (App (left, right), chs)
          | Prez_err _ ->
            (match parse_expr chs with
             | Prez_err _ -> Prez_ok (left, chs)
             | Prez_ok (right, chs) ->
               (* TODO: replace App (left, right) with make_left_prio_app left right *)
               Prez_ok (App (left, right), chs)))))
;;

let test4 () =
  match parse_app (parse_atom parse_evar) (string_to_char_list " ( x ) y ") with
  | Prez_ok (e, tl) ->
    printf "(4) parsed expr: %a\n" (pp_expr true) e
  | Prez_err err ->
    let() = pp_parsing_error stdout err in
     print_endline "(4) parsing error"
;;

let parse_abs parse_expr =
  fix (fun self chs ->
    match skip_char '>' chs with
    | Prez_err err -> Prez_err err
    | Prez_ok ((), chs) ->
      (match parse_var chs with
       | Prez_err err -> Prez_err err
       | Prez_ok (var, chs) ->
         (match skip_char '.' chs with
          | Prez_err err -> Prez_err err
          | Prez_ok ((), chs) ->
            (match self chs with
             | Prez_err err ->
               (match parse_expr chs with
                | Prez_err err -> Prez_err err
                | Prez_ok (body, tl) -> Prez_ok (Abs (var, body), tl))
             | Prez_ok (body, chs) -> Prez_ok (Abs (var, body), chs)))))
;;

let test5 () =
  match
    parse_abs (parse_app (parse_atom parse_evar)) (string_to_char_list "> x . ( x ) y ")
  with
  | Prez_ok (e, tl) ->
    printf "(5) parsed expr: %a\n" (pp_expr true) e
  | Prez_err err ->
    printf "(5) parsing error: %a\n" pp_parsing_error err

;;

let test6 () =
  match
    parse_abs
      (parse_app (parse_atom parse_evar))
      (string_to_char_list "> f . > x . > y . f x y ")
  with
  | Prez_ok (e, tl) ->
    printf "(6) parsed expr: %a\n" (pp_expr true) e
  | Prez_err err ->
    (* printf2 "(6) parsing error: %a\n" *)
     pp_parsing_error stdout err
;;

(* TODO: this one does not work for some reason
  let parse_expr chs =
  fix
    (fun self chs ->
       let parse_atom = parse_atom self in
       let parse_app = parse_app parse_atom in
       let parse_abs = parse_abs parse_app in
       match parse_abs chs with
       | Prez_ok ok -> Prez_ok ok
       | _ ->
         (match parse_app chs with
          | Prez_ok ok -> Prez_ok ok
          | _ -> parse_atom chs))
    chs
;; *)

let parse_expr chs =
  fix
    (fun self chs ->
       match parse_abs (parse_app (parse_atom self)) chs with
       | Prez_ok ok -> Prez_ok ok
       | _ ->
         (match (parse_app (parse_atom self)) chs with
          | Prez_ok ok -> Prez_ok ok
          | _ -> (parse_atom self) chs))
    chs
;;

let test7 () =
  match parse_expr (string_to_char_list "> f . > x . > y . f x y ") with
  | Prez_ok (e, tl) ->
    printf "(7) parsed expr: %a\n" (pp_expr true) e
  | Prez_err err ->
    (* printf2 "(7) parsing error: %a\n"  *)
    pp_parsing_error stdout err
;;

let test8 () =
  match parse_expr (string_to_char_list "(> x . x) a") with
  | Prez_ok (e, tl) ->
    printf "(8) parsed expr: %a\n" (pp_expr true) e
  | Prez_err err ->
      (* printf2 "(8) parsing error: %a\n"  *)
      pp_parsing_error stdout err
;;

let test9 () =
  match parse_expr (string_to_char_list "(> f . > x . f x) (> y . y)") with
  | Prez_ok (e, tl) ->
    printf "(9) parsed expr: %a\n" (pp_expr true) e
  | Prez_err err ->
      (* printf2 "(9) parsing error: %a\n"  *)
      pp_parsing_error stdout err
;;

let test18 () =
  match parse_expr (string_to_char_list "(> k . > x . k) (> x . > y . x)") with
  | Prez_ok (e, tl) ->
    printf "(18) parsed expr: %a\n" (pp_expr true) e
  | Prez_err err ->
    printf "(18) parsing error: %a\n" pp_parsing_error err
;;

let test17 () =
  match parse_expr (string_to_char_list "(> x . > y . > z . x z (y z)) a b c") with
  | Prez_ok (e, tl) ->
    pp_expr true stdout e
    (* printf2 "(17) parsed expr: %a\n" (pp_expr true) e *)
  | Prez_err err ->
     (* printf2 "(17) parsing error: %a\n" *)
   pp_parsing_error stdout err
;;

let test27 =
  match parse_expr (string_to_char_list "(> x . x x) (> x . x x)") with
  | Prez_ok (e, tl) ->
    (* printf2 "(27) Omega: %a\n" *)
    pp_expr true stdout e
  | Prez_err err ->
     (* printf2 "(27) error: %a\n"  *)
  pp_parsing_error stdout err
;;

let test32 =
  match parse_expr (string_to_char_list "> f . > x . f (f x)") with
  | Prez_ok (e, tl) ->
    (* printf2 "(32) Church 2: %a\n"  *)
   pp_expr true stdout e
  | Prez_err err ->
    (* printf2 "(32) error: %a\n"  *)
    pp_parsing_error stdout err
;;

let test34 =
  match parse_expr (string_to_char_list "> m . > n . > f . > x . m f (n f x)") with
  | Prez_ok (e, tl) ->
    (* printf2 "(34) plus: %a\n" (pp_expr true) e *)
    pp_expr true stdout e
  | Prez_err err ->
    (* printf2 "(34) error: %a\n"  *)
    pp_parsing_error stdout err
;;

let test35 =
  match parse_expr (string_to_char_list "> m . > n . > f . m (n f)") with
  | Prez_ok (e, tl) ->
    (* printf2 "(35) mult: %a\n" (pp_expr true) e *)
    pp_expr true stdout e
  | Prez_err err ->
    (* printf2 "(35) error: %a\n"  *)
    pp_parsing_error stdout err
;;

let test37 =
  match parse_expr (string_to_char_list "a b c d e f") with
  | Prez_ok (e, tl) ->
    (* printf2 "(37) chain: %a\n" (pp_expr true) e *)
    pp_expr true stdout e
  | Prez_err err ->
    (* printf2 "(37) error: %a\n" *)
     pp_parsing_error stdout err
;;

let test38 =
  match parse_expr (string_to_char_list "> x . (> y . y) x") with
  | Prez_ok (e, tl) ->
    pp_expr true stdout e
    (* printf2 "(38) nested: %a\n" (pp_expr true) e *)
  | Prez_err err ->
    (* printf2 "(38) error: %a\n" *)
     pp_parsing_error stdout err
;;

let main =
  let () = test0 () in
  let () = test1 () in
  let () = test2 () in
  let () = test3 () in
  let () = test4 () in
  let () = test5 () in
  let () = test6 () in
  let () = test7 () in
  let () = test8 () in
  ()
