(* --- TODO --- *)

let array_of_list lst = [||]
let string_of_char_list chs = "not implemented"

(* ---- AST ----- *)

type constant =
  | Pconst_unit
  | Pconst_int of int (** [ 42 ] *)
  | Pconst_bool of bool (** [ true, false ] *)
  | Pconst_string of string (** [ "string" ] *)

type binop =
  | Pbinop_plus (** [ + ] *)
  | Pbinop_minus (** [ - ] *)
  | Pbinop_asterisk (** [ * ] *)
  | Pbinop_slash (** [ / ] *)
  | Pbinop_eq (** [ = ] *)
  | Pbinop_ne (** [ <> ] *)
  | Pbinop_lt (** [ < ] *)
  | Pbinop_gt (** [ > ] *)
  | Pbinop_le (** [ <= ] *)
  | Pbinop_ge (** [ >= ] *)

type pattern =
  | Ppatt_any
  | Ppatt_var of string
  | Ppatt_const of constant
  | Ppatt_tuple of constant * constant * constant list

type expression =
  | Pexpr_var of string
  | Pexpr_const of constant
  | Pexpr_binop of binop * expression * expression
  | Pexpr_ite of expression * expression * expression
  | Pexpr_tuple of expression * expression * expression list
  | Pexpr_fun of pattern * expression

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
  | x :: xs -> f x :: map f xs
;;

(* ----- char primitives ----- *)

(* char_code is built-in runtime primitive *)
(* math_pow is built-in runtime primitive *)

let is_digit ch = char_code '0' <= char_code ch && char_code ch <= char_code '9'
let is_lowercase ch = char_code 'a' <= char_code ch && char_code ch <= char_code 'z'
let is_uppercase ch = char_code 'A' <= char_code ch && char_code ch <= char_code 'Z'
let int_of_digit ch = char_code ch - char_code '0'

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

(* ----- string primitives ----- *)

let string_len s = length s (* an alias for built-in array primitive *)
let string_nth s n = get s n (* an alias for built-in array primitive *)

(* ----- angstrom-like parser-combinators ----- *)

type parsing_error =
  | Perr_message of string
  | Perr_expected of string
  | Perr_not_implemented of string
  | Perr_unexpected_eof
  | Perr_placeholder

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

let rec many1 p state =
  match p state with
  | Prez_error err -> Prez_error err
  | Prez_success (x, state') ->
    (match many p state' with
     | Prez_error err -> Prez_error err
     | Prez_success (xs, state'') -> return (x :: xs) state'')
;;

let rec choice ps state =
  match ps with
  | [] -> Prez_error (Perr_message "matching no choice")
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
    else Prez_error Perr_placeholder)
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
  else
    (* TODO: replace it with Perr_expected (sprintf "char : %c" ch) *)
    Prez_error (Perr_message "unexpected char")
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

let ws =
  drop_left (many (choice [ char ' '; char '\t'; char '\n'; char '\r' ])) (return ())
;;

let trim p = drop_right (drop_left ws p) ws

let parse_constant =
  choice
    [ map (take_while1 is_digit) (fun chs -> Pexpr_const (Pconst_int (int_of_digits chs)))
    ; map (take_while1 is_char_valid_for_name) (fun chs ->
        Pexpr_var (string_of_char_list chs))
    ; drop_left (string "true") (return (Pexpr_const (Pconst_bool true)))
    ; drop_left (string "false") (return (Pexpr_const (Pconst_bool false)))
    ; drop_left (string "()") (return (Pexpr_const Pconst_unit))
    ]
;;

let parse_expr_binop expr =
  let choice_op cases =
    let rec aux left cases =
      match cases with
      | [] -> fail (Perr_message "no more cases")
      | (op, sep) :: cases' ->
        choice [ drop_left (trim (string sep)) expr; aux left cases' ]
    in
    bind (drop_left ws expr) (fun left -> aux aux cases)
  in
  let cmp expr =
    choice_op
      [ (Pbinop_ne, "<>")
      ; (Pbinop_le, "<=")
      ; (Pbinop_ge, ">=")
      ; (Pbinop_eq, "=")
      ; (Pbinop_lt, "<")
      ; (Pbinop_gt, ">")
      ]
  in
  let add_sub expr = choice_op [ (Pbinop_plus, "+"); (Pbinop_minus, "-") ] in
  let mul_div expr = choice_op [ (Pbinop_asterisk, "*"); (Pbinop_slash, "/") ] in
  list_fold (fun acc expr -> choice [ expr acc; acc ]) [ mul_div; add_sub; cmp ] expr
;;
