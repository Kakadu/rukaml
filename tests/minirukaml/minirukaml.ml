(* --- TODO --- *)

let string_of_char_list s = Base.String.of_char_list s
let rec math_pow b e = if e < 1 then 1 else b * math_pow b (e - 1)
let string_len s = String.length s
let string_nth s n = s.[n]
let is_digit ch = Char.code '0' <= Char.code ch && Char.code ch <= Char.code '9'
let is_lowercase ch = Char.code 'a' <= Char.code ch && Char.code ch <= Char.code 'z'
let is_uppercase ch = Char.code 'A' <= Char.code ch && Char.code ch <= Char.code 'Z'
let int_of_digit ch = Char.code ch - Char.code '0'
let string_of_int = string_of_int
let string_concat = Base.String.concat ~sep:""
let string_concat_sep sep = Base.String.concat ~sep
let list_mem = List.mem
let fprintf = Stdlib.Printf.fprintf

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
  | Ppatt_tuple of pattern * pattern * pattern list

type rec_flag =
  | Pexpr_recursive
  | Pexpr_non_recursive

type expression =
  | Pexpr_var of string
  | Pexpr_const of constant
  | Pexpr_binop of binop * expression * expression
  | Pexpr_ite of expression * expression * expression
  | Pexpr_tuple of expression * expression * expression list
  | Pexpr_fun of pattern * expression
  | Pexpr_app of expression * expression
  | Pexpr_let of rec_flag * pattern * expression * expression

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

let int_of_digits chs =
  let rez, _pos =
    list_fold_right
      (fun digit (acc, pos) -> (acc + (math_pow 10 pos * int_of_digit digit), pos))
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
    let pars = list_mem ctx [ Ctx_tuple; Ctx_fun_lhs ] in
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

let rec pp_expression ctx oc expr =
  match expr with
  | Pexpr_var name -> fprintf oc "%s" name
  | Pexpr_const const -> pp_constant oc const
  | Pexpr_binop (op, e1, e2) ->
    let pars = list_mem ctx [ Ctx_binop; Ctx_app ] in
    fprintf
      oc
      (if pars then "(%a %s %a)" else "%a %s %a")
      (pp_expression Ctx_binop)
      e1
      (show_binop op)
      (pp_expression Ctx_binop)
      e2
  | Pexpr_ite (e1, e2, e3) ->
    let pars = list_mem ctx [ Ctx_binop; Ctx_tuple; Ctx_app; Ctx_ite ] in
    fprintf
      oc
      (if pars then "(if %a then %a else %a)" else "if %a then %a else %a")
      (pp_expression Ctx_ite)
      e1
      (pp_expression Ctx_ite)
      e2
      (pp_expression Ctx_ite)
      e3
  | Pexpr_tuple (e1, e2, es) ->
    let pars = list_mem ctx [ Ctx_binop; Ctx_tuple; Ctx_app ] in
    fprintf
      oc
      (if pars then "(%a)" else "%a")
      (pp_tuple (pp_expression Ctx_tuple))
      (e1, e2, es)
  | Pexpr_fun (p, e) ->
    let pars = list_mem ctx [ Ctx_binop; Ctx_tuple; Ctx_app ] in
    fprintf
      oc
      (if pars then "(fun %a -> %a)" else "fun %a -> %a")
      (pp_pattern Ctx_fun_lhs)
      p
      (pp_expression Ctx_fun_rhs)
      e
  | Pexpr_app (e1, e2) ->
    let pars = list_mem ctx [ Ctx_app ] in
    fprintf
      oc
      (if pars then "(%a %a)" else "%a %a")
      (pp_expression Ctx_app)
      e1
      (pp_expression Ctx_app)
      e2
  | Pexpr_let (rec_flag, lhs, rhs, body) ->
    let pars = list_mem ctx [ Ctx_binop; Ctx_tuple; Ctx_app; Ctx_ite; Ctx_let_rhs ] in
    fprintf
      oc
      (match (pars, rec_flag) with
       | true, Pexpr_non_recursive -> "(let %a = %a in %a)"
       | false, Pexpr_non_recursive -> "let %a = %a in %a"
       | true, Pexpr_recursive -> "(let rec %a = %a in %a)"
       | false, Pexpr_recursive -> "let rec %a = %a in %a")
      (pp_pattern Ctx_let_lhs)
      lhs
      (pp_expression Ctx_let_rhs)
      rhs
      (pp_expression Ctx_let_body)
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
  | Prez_success of 'a * parser_state
  | Prez_error of parsing_error

type 'a parser = parser_state -> 'a parsing_result

let return x state = Prez_success (x, state)
let fail err _state = Prez_error err

let bind p f state =
  match p state with
  | Prez_error err -> Prez_error err
  | Prez_success (x, state') -> f x state'
;;

let map p f state =
  match p state with
  | Prez_error err -> Prez_error err
  | Prez_success (rez, state) -> return (f rez) state
;;

let many p state =
  let rec aux acc state =
    match p state with
    | Prez_success (rez, state') -> aux (rez :: acc) state'
    | Prez_error _ -> Prez_success (list_rev acc, state)
  in
  aux [] state
;;

let many1 p state =
  match p state with
  | Prez_error err -> Prez_error err
  | Prez_success (x, state') ->
    (match many p state' with
     | Prez_error err -> Prez_error err
     | Prez_success (xs, state'') -> return (x :: xs) state'')
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
     | Prez_success (rez, state') -> Prez_success (rez, state')
     | Prez_error _ -> choice ps state)
;;

let take_while pred state =
  let rec aux (str, pos) acc =
    if pos >= string_len str
    then return (list_rev acc) (str, pos)
    else (
      let ch = string_nth str pos in
      if pred ch then aux (str, pos + 1) (ch :: acc) else return (list_rev acc) (str, pos))
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
  | Prez_success (_, state') -> p2 state'
;;

let drop_right p1 p2 state =
  match p1 state with
  | Prez_error err -> Prez_error err
  | Prez_success (rez, state') ->
    (match p2 state' with
     | Prez_error err -> Prez_error err
     | Prez_success (_, state'') -> return rez state'')
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
     | Prez_success (_, state') -> drop_many ps p state')
;;

(* ----- parser implementation ----- *)

type 'a fix = Fix of ('a fix -> 'a)

let fix f =
  let rec g (Fix x) = f (fun a -> g (Fix x) a) in
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
         map (many (drop_left (trim (char ',')) parse_item)) (fun xs -> (x1, x2, xs)))))
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
      [ (Pbinop_ne, "<>")
      ; (Pbinop_le, "<=")
      ; (Pbinop_ge, ">=")
      ; (Pbinop_eq, "=")
      ; (Pbinop_lt, "<")
      ; (Pbinop_gt, ">")
      ]
  in
  let add_sub expr = make_binop_level expr [ (Pbinop_add, "+"); (Pbinop_sub, "-") ] in
  let mul_div expr = make_binop_level expr [ (Pbinop_mul, "*"); (Pbinop_div, "/") ] in
  let l_and expr = make_binop_level expr [ (Pbinop_land, "&&") ] in
  let l_or expr = make_binop_level expr [ (Pbinop_lor, "||") ] in
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

let pp_expression = pp_expression Ctx_free

let parse_expression s =
  match parse_expr (s, 0) with
  | Prez_success (ast, _state) -> Ok ast
  | Prez_error err -> Error err
;;
