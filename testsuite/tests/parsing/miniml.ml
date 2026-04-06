(*
   test
  (targets amd64)
  (run
    (stdout
      "rukaml_print_int 1000"
      "rukaml_print_int 100"
      "rukaml_print_int 1"
      "rukaml_print_int 10"
      "rukaml_print_int 100"))
*)

(* [begin skip] *)

let string_of_char_list chs = String.of_seq (List.to_seq chs)
let string_len s = String.length s
let string_nth s n = s.[n]
let char_code = Char.code
let printf, fprintf, sprintf = Stdlib.Printf.(printf, fprintf, sprintf)

(* [end skip] *)

(* ----- list primitives ----- *)

let list_rev ls =
  let rec aux ls acc =
    match ls with
    | [] -> acc
    | x :: xs -> aux xs (x :: acc)
  in
  aux ls []
;;

let rec list_fold f ls init =
  match ls with
  | [] -> init
  | x :: xs -> list_fold f xs (f init x)
;;

let rec list_fold_right f ls init =
  match ls with
  | [] -> init
  | x :: xs -> f x (list_fold_right f xs init)
;;

let rec list_map f ls =
  match ls with
  | [] -> []
  | x :: xs -> f x :: list_map f xs
;;

let rec list_iter f ls =
  match ls with
  | [] -> ()
  | x :: xs ->
    let () = f x in
    list_iter f xs
;;

(* ---- AST ----- *)

type constant =
  | Pconst_unit
  | Pconst_int of int (** [ 42 ] *)
  | Pconst_bool of bool (** [ true, false ] *)
  | Pconst_string of string (** [ "string" ] *)

type binop =
  | Pbinop_add (** [ + ] *)
  | Pbinop_sub (** [ - ] *)
  | Pbinop_mul (** [ * ] *)
  | Pbinop_div (** [ / ] *)
  | Pbinop_eq (** [ = ] *)
  | Pbinop_ne (** [ <> ] *)
  | Pbinop_lt (** [ < ] *)
  | Pbinop_gt (** [ > ] *)
  | Pbinop_le (** [ <= ] *)
  | Pbinop_ge (** [ >= ] *)
  | Pbinop_lor (** [ || ] *)
  | Pbinop_land (** [ && ] *)

type pattern =
  | Ppatt_any
  | Ppatt_var of string
  | Ppatt_const of constant
  | Ppatt_tuple of (pattern * pattern * pattern list)

type rec_flag =
  | Pexpr_recursive
  | Pexpr_non_recursive

type expression =
  | Pexpr_var of string
  | Pexpr_const of constant
  | Pexpr_binop of (binop * expression * expression)
  | Pexpr_ite of (expression * expression * expression)
  | Pexpr_tuple of (expression * expression * expression list)
  | Pexpr_fun of (pattern * expression)
  | Pexpr_app of (expression * expression)
  | Pexpr_let of (rec_flag * pattern * expression * expression)

let is_keyword s =
  match s with
  | "let" -> true
  | "rec" -> true
  | "in" -> true
  | "true" -> true
  | "false" -> true
  | "fun" -> true
  | "if" -> true
  | "then" -> true
  | "else" -> true
  | _ -> false
;;

(* ----- char primitives ----- *)

let is_lowercase ch = char_code 'a' <= char_code ch && char_code ch <= char_code 'z'
let is_uppercase ch = char_code 'A' <= char_code ch && char_code ch <= char_code 'Z'
let is_digit ch = char_code '0' <= char_code ch && char_code ch <= char_code '9'
let int_of_digit ch = char_code ch - char_code '0'

let int_of_digits chs =
  let rec pow b e = if e < 1 then 1 else b * pow b (e - 1) in
  let rez, _pos =
    list_fold_right
      (fun digit (acc, pos) -> acc + (pow 10 pos * int_of_digit digit), pos)
      chs
      (0, 1)
  in
  rez
;;

let is_char_valid_for_name ch =
  is_digit ch || is_lowercase ch || is_uppercase ch || ch = '_' || ch = '\''
;;

(* ----- AST pretty-printers ----- *)

let pp_tuple pp_item oc (x1, x2, xs) =
  let () = fprintf oc "%a, %a" pp_item x1 pp_item x2 in
  list_iter (fun x -> fprintf oc ", %a" pp_item x) xs
;;

let pp_constant oc const =
  match const with
  | Pconst_unit -> fprintf oc "()"
  | Pconst_int n -> fprintf oc "%d" n
  | Pconst_bool true -> fprintf oc "true"
  | Pconst_bool false -> fprintf oc "false"
  | Pconst_string s -> fprintf oc "\"%s\"" s
;;

let show_binop binop =
  match binop with
  | Pbinop_add -> "+"
  | Pbinop_sub -> "-"
  | Pbinop_mul -> "*"
  | Pbinop_div -> "/"
  | Pbinop_eq -> "="
  | Pbinop_ne -> "<>"
  | Pbinop_lt -> "<"
  | Pbinop_gt -> ">"
  | Pbinop_le -> "<="
  | Pbinop_ge -> ">="
  | Pbinop_lor -> "||"
  | Pbinop_land -> "&&"
;;

let pp_binop oc binop = fprintf oc "%s" (show_binop binop)

type pp_pattern_ctx =
  | Ctx_tuple (* <here>, <here> *)
  | Ctx_let_lhs (* let <here> = ... in ... *)
  | Ctx_fun_lhs (* fun <here> -> ... *)
  | Ctx_free (* for testing purposes *)

let rec pp_pattern ctx oc patt =
  match patt with
  | Ppatt_any -> fprintf oc "_"
  | Ppatt_var name -> fprintf oc "%s" name
  | Ppatt_const c -> pp_constant oc c
  | Ppatt_tuple (p1, p2, ps) ->
    let pars =
      match ctx with
      | Ctx_tuple -> true
      | Ctx_fun_lhs -> true
      | _ -> false
    in
    fprintf
      oc
      (if pars then "(%a)" else "%a")
      (pp_tuple (pp_pattern Ctx_tuple))
      (p1, p2, ps)
;;

type pp_expression_ctx =
  | Ctx_binop (* <here> + <here> *)
  | Ctx_tuple (* <here>, <here> *)
  | Ctx_app (* <here> <here> *)
  | Ctx_ite (* if <here> then <here> else <here> *)
  | Ctx_let_rhs (* let ... = <here> in ... *)
  | Ctx_let_body (* let ... = ... in <here> *)
  | Ctx_fun_rhs (* fun ... -> <here> *)
  | Ctx_free (* for testing purposes *)

let rec pp_expr ctx oc expr =
  match expr with
  | Pexpr_var name -> fprintf oc "%s" name
  | Pexpr_const const -> pp_constant oc const
  | Pexpr_binop (op, e1, e2) ->
    let pars =
      match ctx with
      | Ctx_binop -> true
      | Ctx_app -> true
      | _ -> false
    in
    fprintf
      oc
      (if pars then "(%a %s %a)" else "%a %s %a")
      (pp_expr Ctx_binop)
      e1
      (show_binop op)
      (pp_expr Ctx_binop)
      e2
  | Pexpr_ite (e1, e2, e3) ->
    let pars =
      match ctx with
      | Ctx_binop -> true
      | Ctx_tuple -> true
      | Ctx_app -> true
      | Ctx_ite -> true
      | _ -> false
    in
    fprintf
      oc
      (if pars then "(if %a then %a else %a)" else "if %a then %a else %a")
      (pp_expr Ctx_ite)
      e1
      (pp_expr Ctx_ite)
      e2
      (pp_expr Ctx_ite)
      e3
  | Pexpr_tuple (e1, e2, es) ->
    let pars =
      match ctx with
      | Ctx_binop -> true
      | Ctx_tuple -> true
      | Ctx_app -> true
      | _ -> false
    in
    fprintf oc (if pars then "(%a)" else "%a") (pp_tuple (pp_expr Ctx_tuple)) (e1, e2, es)
  | Pexpr_fun (p, e) ->
    let pars =
      match ctx with
      | Ctx_binop -> true
      | Ctx_tuple -> true
      | Ctx_app -> true
      | _ -> false
    in
    fprintf
      oc
      (if pars then "(fun %a -> %a)" else "fun %a -> %a")
      (pp_pattern Ctx_fun_lhs)
      p
      (pp_expr Ctx_fun_rhs)
      e
  | Pexpr_app (e1, e2) ->
    let pars =
      match ctx with
      | Ctx_app -> true
      | _ -> false
    in
    fprintf
      oc
      (if pars then "(%a %a)" else "%a %a")
      (pp_expr Ctx_app)
      e1
      (pp_expr Ctx_app)
      e2
  | Pexpr_let (rec_flag, lhs, rhs, body) ->
    let pars =
      match ctx with
      | Ctx_binop -> true
      | Ctx_tuple -> true
      | Ctx_app -> true
      | Ctx_ite -> true
      | Ctx_let_rhs -> true
      | _ -> false
    in
    fprintf
      oc
      (match pars, rec_flag with
       | true, Pexpr_non_recursive -> "(let %a = %a in %a)"
       | false, Pexpr_non_recursive -> "let %a = %a in %a"
       | true, Pexpr_recursive -> "(let rec %a = %a in %a)"
       | false, Pexpr_recursive -> "let rec %a = %a in %a")
      (pp_pattern Ctx_let_lhs)
      lhs
      (pp_expr Ctx_let_rhs)
      rhs
      (pp_expr Ctx_let_body)
      body
;;

(* ----- angstrom-like parser-combinators ----- *)

type parsing_error =
  | Perr_message of string
  | Perr_expected of string
  | Perr_not_implemented of string
  | Perr_unexpected_eof

type parser_state = string * int (* int stands for current position *)

type 'a parsing_result =
  | Prez_success of ('a * parser_state)
  | Prez_error of parsing_error

type 'a parser = parser_state -> 'a parsing_result

let return x state = Prez_success (x, state)
let fail err _state = Prez_error err

let bind p f state =
  match p state with
  | Prez_error err -> Prez_error err
  | Prez_success (x, state2) -> f x state2
;;

let map p f state =
  match p state with
  | Prez_error err -> Prez_error err
  | Prez_success (rez, state) -> return (f rez) state
;;

let many p state =
  let rec aux acc state =
    match p state with
    | Prez_success (rez, state2) -> aux (rez :: acc) state2
    | Prez_error _ -> Prez_success (list_rev acc, state)
  in
  aux [] state
;;

let many1 p state =
  match p state with
  | Prez_error err -> Prez_error err
  | Prez_success (x, state2) ->
    (match many p state2 with
     | Prez_error err -> Prez_error err
     | Prez_success (xs, state3) -> return (x :: xs) state3)
;;

let choice2 p1 p2 state =
  match p1 state with
  | Prez_error _ -> p2 state
  | success -> success
;;

let rec choice ps state =
  match ps with
  | [] -> Prez_error (Perr_message "choice")
  | p :: ps ->
    (match p state with
     | Prez_success (rez, state2) -> Prez_success (rez, state2)
     | Prez_error _ -> choice ps state)
;;

let take_while pred state =
  let rec aux state acc =
    match state with
    (* TODO: allow tuples as arg to fix this *)
    | str, pos ->
      if pos >= string_len str
      then return (list_rev acc) (str, pos)
      else (
        let ch = string_nth str pos in
        if pred ch
        then aux (str, pos + 1) (ch :: acc)
        else return (list_rev acc) (str, pos))
  in
  aux state []
;;

let take_while1 pred (str, pos) =
  if pos >= string_len str
  then Prez_error Perr_unexpected_eof
  else (
    let ch1 = string_nth str pos in
    if pred ch1
    then map (take_while pred) (fun chs -> ch1 :: chs) (str, pos + 1)
    else Prez_error (Perr_message "take_while1"))
;;

let drop_left p1 p2 state =
  match p1 state with
  | Prez_error err -> Prez_error err
  | Prez_success (_, state2) -> p2 state2
;;

let drop_right p1 p2 state =
  match p1 state with
  | Prez_error err -> Prez_error err
  | Prez_success (rez, state2) ->
    (match p2 state2 with
     | Prez_error err -> Prez_error err
     | Prez_success (_, state3) -> return rez state3)
;;

let char ch (str, pos) =
  if pos >= string_len str
  then Prez_error Perr_unexpected_eof
  else if string_nth str pos = ch
  then return ch (str, pos + 1)
  else Prez_error (Perr_message "unexpected char")
;;

let string expected (str, pos) =
  let rec aux i =
    if i >= string_len expected
    then return expected (str, pos + i)
    else if pos + i >= string_len str
    then Prez_error Perr_unexpected_eof
    else if string_nth str (pos + i) = string_nth expected i
    then aux (i + 1)
    else Prez_error (Perr_message "unexpected string")
  in
  aux 0
;;

let rec drop_many ps p state =
  match ps with
  | [] -> p state
  | p1 :: ps ->
    (match p1 state with
     | Prez_error err -> Prez_error err
     | Prez_success (_, state2) -> drop_many ps p state2)
;;

(* ----- parser implementation ----- *)

type 'a fix = Fix of ('a fix -> 'a)

let fix f =
  let rec g x =
    match x with
    | Fix x -> f (fun a -> g (Fix x) a)
  in
  g (Fix g)
;;

let ws =
  drop_left (many (choice [ char ' '; char '\t'; char '\n'; char '\r' ])) (return ())
;;

let skip_ws p = drop_left ws p
let trim p = drop_right (drop_left ws p) ws
let parens p = drop_left (trim (char '(')) (drop_right p (trim (char ')')))

let parse_constant =
  choice
    [ map (take_while1 is_digit) (fun chs -> Pconst_int (int_of_digits chs))
    ; drop_left (string "true") (return (Pconst_bool true))
    ; drop_left (string "false") (return (Pconst_bool false))
    ; drop_left (string "()") (return Pconst_unit)
    ]
;;

let parse_var_name =
  bind (take_while1 is_char_valid_for_name) (fun chs ->
    let name = string_of_char_list chs in
    if is_keyword name then fail (Perr_message "unexpected keyword") else return name)
;;

let parse_tuple parse_item =
  bind parse_item (fun x1 ->
    drop_left
      (drop_left ws (drop_left (char ',') ws))
      (bind parse_item (fun x2 ->
         map (many (drop_left (trim (char ',')) parse_item)) (fun xs -> x1, x2, xs))))
;;

let parse_pattern_atom =
  skip_ws
    (choice
       [ drop_left (char '_') (return Ppatt_any)
       ; map parse_var_name (fun name -> Ppatt_var name)
       ; map parse_constant (fun c -> Ppatt_const c)
       ])
;;

let parse_pattern =
  fix (fun self ->
    let basic = choice [ parse_pattern_atom; parens self ] in
    let tuple = map (parse_tuple basic) (fun (x1, x2, xs) -> Ppatt_tuple (x1, x2, xs)) in
    choice [ tuple; basic ])
;;

let parse_expr_atom =
  skip_ws
    (choice
       [ map parse_var_name (fun name -> Pexpr_var name)
       ; map parse_constant (fun c -> Pexpr_const c)
       ])
;;

let make_binop_level expr cases =
  let make_parser (op, sep) =
    drop_left
      (skip_ws (string sep))
      (map expr (fun right left -> Pexpr_binop (op, left, right)))
  in
  let level = choice (list_map make_parser cases) in
  bind expr (fun init ->
    map (many1 level) (fun fs -> list_fold (fun acc f -> f acc) fs init))
;;

let parse_expr_binop expr =
  let cmp expr =
    make_binop_level
      expr
      [ Pbinop_ne, "<>"
      ; Pbinop_le, "<="
      ; Pbinop_ge, ">="
      ; Pbinop_eq, "="
      ; Pbinop_lt, "<"
      ; Pbinop_gt, ">"
      ]
  in
  let add_sub expr = make_binop_level expr [ Pbinop_add, "+"; Pbinop_sub, "-" ] in
  let mul_div expr = make_binop_level expr [ Pbinop_mul, "*"; Pbinop_div, "/" ] in
  let l_and expr = make_binop_level expr [ Pbinop_land, "&&" ] in
  let l_or expr = make_binop_level expr [ Pbinop_lor, "||" ] in
  list_fold
    (fun acc level -> choice2 (level acc) acc)
    [ mul_div; add_sub; cmp; l_and; l_or ]
    expr
;;

let parse_expr_app parse_expr =
  bind
    (skip_ws (many (skip_ws parse_expr)))
    (fun exprs ->
       match exprs with
       | [] -> fail (Perr_expected "expr_app (or expr_basic)")
       | e :: es -> return (list_fold (fun f x -> Pexpr_app (f, x)) es e))
;;

let parse_expr_ite parse_expr =
  drop_left
    (trim (string "if"))
    (bind parse_expr (fun e1 ->
       drop_left
         (trim (string "then"))
         (bind parse_expr (fun e2 ->
            drop_left
              (trim (string "else"))
              (bind parse_expr (fun e3 -> return (Pexpr_ite (e1, e2, e3))))))))
;;

let parse_expr_fun parse_expr =
  drop_left
    (trim (string "fun"))
    (bind (many1 parse_pattern) (fun ps ->
       drop_left
         (trim (string "->"))
         (bind parse_expr (fun b ->
            let desugared =
              (* presents [ fun x y z -> ... ] as [ fun x -> fun y -> fun z -> ... ] *)
              list_fold_right (fun p e -> Pexpr_fun (p, e)) ps b
            in
            return desugared))))
;;

let parse_rec_flag =
  choice
    [ drop_left (skip_ws (string "rec")) (return Pexpr_recursive)
    ; return Pexpr_non_recursive
    ]
;;

let parse_expr_let parse_expr =
  drop_left
    (trim (string "let"))
    (bind parse_rec_flag (fun rec_flag ->
       bind parse_pattern (fun p1 ->
         bind (many parse_pattern) (fun ps ->
           drop_left
             (trim (char '='))
             (bind parse_expr (fun rhs ->
                drop_left
                  (trim (string "in"))
                  (bind parse_expr (fun body ->
                     let desugared =
                       (* presents [ let f x y = ... in ... ] as [ let f = fun x -> fun y -> ... in ... ]*)
                       list_fold_right (fun p acc -> Pexpr_fun (p, acc)) ps rhs
                     in
                     return (Pexpr_let (rec_flag, p1, desugared, body))))))))))
;;

let parse_expr_complex expr =
  fix (fun self ->
    choice [ parse_expr_ite self; parse_expr_let self; parse_expr_fun self; expr ])
;;

let parse_expr_tuple expr =
  bind (parse_expr_complex expr) (fun e1 ->
    drop_left
      (trim (char ','))
      (bind (parse_expr_complex expr) (fun e2 ->
         bind
           (many (drop_left (trim (char ',')) (parse_expr_complex expr)))
           (fun es -> return (Pexpr_tuple (e1, e2, es))))))
;;

let parse_expr =
  fix (fun self ->
    let basic = choice [ parse_expr_atom; parens self ] in
    let ps = [ parse_expr_app; parse_expr_binop; parse_expr_tuple; parse_expr_complex ] in
    list_fold (fun acc expr -> choice2 (expr acc) acc) ps basic)
;;

(* run *)

type ('a, 'b) result =
  | Ok of 'a
  | Error of 'b

let pp_parsing_error oc err =
  match err with
  | Perr_message msg -> fprintf oc "%s" msg
  | Perr_expected x -> fprintf oc "expected: %s" x
  | Perr_not_implemented x -> fprintf oc "not implemented: %s" x
  | Perr_unexpected_eof -> fprintf oc "unexpected eof"
;;

let pp_expression oc e = pp_expr Ctx_free oc e

let parse_expression s =
  match parse_expr (s, 0) with
  | Prez_success (ast, _state) -> Ok ast
  | Prez_error err -> Error err
;;

(* driver *)

let run_single input =
  match parse_expression input with
  | Error err -> printf "parsing error: %a\n" pp_parsing_error err
  | Ok ast -> printf "parsed: %a\n" pp_expression ast
;;

(* tests *)

let break666 () = ()
let test001 = run_single "1"
let test002 = run_single "true"
let test003 = run_single "false"
let test004 = run_single "()"
let test005 = run_single "x"
let test006 = run_single "x + y"
let test007 = run_single "x - y"
let test008 = run_single "x * y"
let test009 = run_single "x / y"
let test010 = run_single "x = y"
let test011 = run_single "x < y"
let test012 = run_single "x <= y"
let test013 = run_single "x > y"
let test014 = run_single "x >= y"
let test015 = run_single "x <> y"
let test016 = run_single "x && y"
let test017 = run_single "x || y"
let test018 = run_single "if true then 1 else 0"
let test019 = run_single "if x < 1 then y else z"
let test020 = run_single "fun x -> x"
let test021 = run_single "let x = 1 in x"
let test022 = run_single "let x = 1 in let y = 2 in x + y"
let test023 = run_single "let rec f = fun n -> n in f"
let test024 = run_single "(1, 2)"
let test025 = run_single "(1, 2, 3)"
let test026 = run_single "1 + 2 * 3"
let test027 = run_single "(1 + 2) * 3"
let test028 = run_single "let f = fun x -> x + 1 in f 5"
let test030 = run_single "let x = 1 in let y = 2 in let z = 3 in x + y + z"
let test047 = run_single "(1, 2, 3, 4)"
let test048 = run_single "let x = 10 in let y = 20 in let z = 30 in (x + y, y + z, x + z)"
let test049 = run_single "fun a -> fun b -> fun c -> fun d -> a + b + c + d"
let test051 = run_single "let f = fun x -> let g = fun y -> x + y in g in f 1 2"

let test052 =
  run_single
    "let x = 42 in let f = fun x -> fun y -> x * y in let g = fun y -> y + 1 in f x (g x)"
;;

let test053 =
  run_single
    "let a = 1 in let b = 2 in let c = 3 in let d = 4 in let e = 5 in a + b + c + d + e"
;;

let test055 = run_single "let swap = fun p -> let x = 1 in let y = 2 in (y, x) in swap 0"
let test056 = run_single "let f = fun x -> (x, x + 1, x + 2) in f 5"
let test058 = run_single "let x = 1 in let f = fun x -> x in let g = fun f -> f 1 in g f"

let test_fact =
  run_single "let rec fact n = if n < 1 then 1 else n * fact (n - 1) in fact 5"
;;

let testx001 = run_single "x y z"



(*
   TODO: SIGILL
let test050 =
  run_single
    "let rec ack = fun m -> fun n -> if m = 0 then n + 1 else if n = 0 then ack (m - 1) 1 else ack (m - 1) (ack m (n - 1)) in ack 3 4"
;;

let test054 =
  run_single
    "let rec map = fun f -> fun l -> if l = 0 then 0 else f (map f (l - 1)) in map (fun x -> x + 1) 10"
;;

let test059 =
  run_single
    "let x = 0 in let f = fun x -> x + 1 in let g = fun x -> x + 2 in let h = fun x -> x + 3 in f (g (h x))"
;;

let test057 =
  run_single
    "let rec gcd = fun a -> fun b -> if b = 0 then a else gcd b (a mod b) in gcd 48 18"
;;

let test060 =
  run_single
    "let rec range = fun a -> fun b -> if a > b then 0 else 1 + range (a + 1) b in range 1 10"
;;

let test_fib =
  run_single "let rec fib n = if n < 2 then n else fib (n - 1) + fib (n - 2) in fib 5"
;;

let test_fact2 =
  run_single
    "let rec fact n acc = if n < 1 then acc else fact (n - 1) (n * acc) in fact 5 0"
;;
*)

let main = break666 ()
