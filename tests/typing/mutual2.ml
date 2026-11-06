let failwith msg =
  let () = fprintf stderr "[compiler] error: %s \n" msg in
  exit 1
;;

(* list primitives *)

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

let rec list_length ls =
  match ls with
  | [] -> 0
  | _ :: xs -> 1 + list_length xs
;;

let is_lparen ch = ch = '('
let is_rparen ch = ch = ')'
let is_comma ch = ch = ','

let is_lowercase ch =
  let c = char_code ch in
  char_code 'a' <= c && c <= char_code 'z'
;;

let is_uppercase ch =
  let c = char_code ch in
  char_code 'A' <= c && c <= char_code 'Z'
;;

let is_digit ch =
  let c = char_code ch in
  char_code '0' <= c && c <= char_code '9'
;;

let is_alphanum ch = is_lowercase ch || is_uppercase ch || is_digit ch || ch = '_'
let int_of_digit ch = char_code ch - char_code '0'

let int_of_digits chs =
  let rec aux acc xs =
    match xs with
    | [] -> acc
    | h :: tl -> aux ((acc * 10) + int_of_digit h) tl
  in
  match chs with
  | [] -> 0
  | h :: tl -> aux (int_of_digit h) tl
;;

(* ast *)

type 'a option =
  | Some of 'a
  | None

type ptype =
  | Ptype_int
  | Ptype_void

type pbinop =
  | Pbinop_add
  | Pbinop_sub
  | Pbinop_mul
  | Pbinop_div
  | Pbinop_eq
  | Pbinop_ne
  | Pbinop_lt
  | Pbinop_gt
  | Pbinop_le
  | Pbinop_ge
  | Pbinop_and
  | Pbinop_or

type pexpression =
  | Pexpr_int of int
  | Pexpr_var of string
  | Pexpr_call of string * pexpression list
  | Pexpr_binop of pbinop * pexpression * pexpression
  | Pexpr_tern of pexpression * pexpression * pexpression
  | Pexpr_assign of string * pexpression

type pdecl = Pdecl_var of ptype * (string * pexpression option) list

type pstatement =
  | Pstmt_return of pexpression option
  | Pstmt_expr of pexpression
  | Pstmt_block of pstatement list
  | Pstmt_decl of pdecl
  | Pstmt_ite of pexpression * pstatement * pstatement option
  | Pstmt_for of
      pdecl option * pexpression option * pexpression option * pstatement option

type pprogram_item =
  | Pitem_function of string * ptype * (ptype * string) list * pstatement list

type pprogram = pprogram_item list

let is_cpp_keyword s =
  match s with
  | "int" -> true
  | "void" -> true
  | "if" -> true
  | "else" -> true
  | "for" -> true
  | "while" -> true
  | _ -> false
;;

(* parser combinators *)

type parser_state = int

type parsing_error =
  | Perr_message of string
  | Perr_expected of string
  | Perr_unexpected_eof

let sarr = [| "" |]

type 'a parsing_result =
  | Prez_success of 'a * parser_state
  | Prez_error of parsing_error

type 'a parser = parser_state -> 'a parsing_result

let return x state = Prez_success (x, state)
let fail err _state = Prez_error err

let bind p f state =
  (* let (_,pos) = state in
  let () = printf "\ncalling bind on pos %d\n" pos in
  let () = trace_rukaml_val state in *)
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
    | Prez_error _ ->
      let ans = list_rev acc in
      Prez_success (ans, state)
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

let choice_err = Perr_message "choice"

let rec choice ps state =
  match ps with
  | [] -> Prez_error choice_err
  | p :: ps ->
    let ans = p state in
    (match ans with
     | Prez_success (rez, state2) -> ans
     | Prez_error _ -> choice ps state)
;;

let take_while pred state =
  let rec aux pos acc =
    if pos >= string_len sarr.(0)
    then return (list_rev acc) pos
    else (
      let ch = string_nth sarr.(0) pos in
      if pred ch then aux (pos + 1) (ch :: acc) else return (list_rev acc) pos)
  in
  aux state []
;;

let skip_while pred pos =
  let rec aux pred pos =
    if pos >= string_len sarr.(0)
    then return () pos
    else (
      let ch = string_nth sarr.(0) pos in
      if pred ch then aux pred (pos + 1) else return () pos)
  in
  aux pred pos
;;

let take_while1 pred pos =
  if pos >= string_len sarr.(0)
  then Prez_error Perr_unexpected_eof
  else (
    let ch1 = string_nth sarr.(0) pos in
    if pred ch1
    then map (take_while pred) (fun chs -> ch1 :: chs) (pos + 1)
    else Prez_error (Perr_message "take_while1"))
;;

let rec take_while1_string_aux pred curpos =
  let s = sarr.(0) in
  if pred (string_nth s curpos) then take_while1_string_aux pred (1 + curpos) else curpos
;;

let take_while1_string_err = Perr_message "take_while1_string"

let take_while1_string pred pos =
  let rpos = take_while1_string_aux pred pos in
  if rpos > pos
  then (
    let ans = substring sarr.(0) pos (rpos - pos) in
    Prez_success (ans, rpos))
  else Prez_error take_while1_string_err
;;

let drop_left p1 p2 state =
  (*
     let (_,pos) = state in
  let () = printf "calling drop_left on pos %d\n" pos in*)
  match p1 state with
  | Prez_error err -> Prez_error err
  | Prez_success (_, state2) -> p2 state2
;;

let drop_right p1 p2 state =
  let ans = p1 state in
  match ans with
  | Prez_error err -> ans
  | Prez_success (rez, state2) ->
    (match p2 state2 with
     | Prez_error err -> Prez_error err
     | Prez_success (_, state3) -> return rez state3)
;;

let char ch pos =
  if pos >= string_len sarr.(0)
  then Prez_error Perr_unexpected_eof
  else (
    let ch0 = string_nth sarr.(0) pos in
    (* let() = printf "Trying to parse '%c' at pos %d = '%c'\n" ch pos ch0 in *)
    if ch0 = ch then return ch (pos + 1) else Prez_error (Perr_message "unexpected char"))
;;

let lparen pos = char '(' pos
let rparen pos = char ')' pos
let comma pos = char ',' pos
let string_err = Perr_message "unexpected string"

let string expected pos =
  let rec aux explen len i =
    if i >= explen
    then
      (* let () = printf "string '%s' parsed\n" expected in *)
      return expected (pos + i)
    else if pos + i >= len
    then Prez_error Perr_unexpected_eof
    else (
      let c1 = string_nth sarr.(0) (pos + i) in
      let c2 = string_nth expected i in
      if c1 = c2 then aux explen len (i + 1) else Prez_error string_err)
  in
  (* let () = printf "inside string  parser.\n" in *)
  (*
  let () = printf "inside string  parser at pos %d\n" pos in
  let () = trace_rukaml_val state in
  let () = printf "expected ='%s'\n" expected in *)
  aux (string_len expected) (string_len sarr.(0)) 0
;;

let rec drop_many ps p state =
  match ps with
  | [] -> p state
  | p1 :: ps ->
    (match p1 state with
     | Prez_error err -> Prez_error err
     | Prez_success (_, state2) -> drop_many ps p state2)
;;

(* parser implementation *)

let ws pos =
  let rec helper len pos =
    if pos >= len
    then Prez_success ((), pos)
    else (
      let ch = string_nth sarr.(0) pos in
      let cond = ch = ' ' || ch = '\n' in
      if cond then helper len (pos + 1) else Prez_success ((), pos))
  in
  helper (string_len sarr.(0)) pos
;;

let skip_ws p state =
  match ws state with
  | Prez_success (_, st0) -> p st0
  | Prez_error _ -> exit 2
;;

let ws_exn st =
  match ws st with
  | Prez_success (_, st0) -> st0
  | _ -> exit 44
;;

let trim p st =
  let st = ws_exn st in
  let ans = p st in
  match ans with
  | Prez_error e -> ans
  | Prez_success (x, st0) ->
    let st1 = ws_exn st0 in
    Prez_success (x, st1)
;;

(* drop_right (drop_left ws p) ws *)

let parens p st =
  let st = ws_exn st in
  let ans1 = trim lparen st in
  match ans1 with
  | Prez_error e -> Prez_error e
  | Prez_success (_, st1) ->
    let st1 = ws_exn st1 in
    let ans2 = p st1 in
    (match ans2 with
     | Prez_error e -> Prez_error e
     | Prez_success (x, st2) ->
       let st3 = ws_exn st2 in
       let ans3 = trim rparen st3 in
       (match ans3 with
        | Prez_error e -> Prez_error e
        | Prez_success (_, st3) ->
          let st3 = ws_exn st3 in
          Prez_success (x, st3)))
;;

(*
   drop_left (trim lparen) (drop_right p (trim (rparen))) st *)

let braces p st =
  let st = ws_exn st in
  let ans1 = trim (char '{') st in
  match ans1 with
  | Prez_error e -> Prez_error e
  | Prez_success (_, st1) ->
    let st1 = ws_exn st1 in
    let ans2 = p st1 in
    (match ans2 with
     | Prez_error e -> Prez_error e
     | Prez_success (x, st2) ->
       let st3 = ws_exn st2 in
       let ans3 = trim (char '}') st3 in
       (match ans3 with
        | Prez_error e -> Prez_error e
        | Prez_success (_, st3) ->
          let st3 = ws_exn st3 in
          Prez_success (x, st3)))
;;

(*
   drop_left (trim (char '{')) (drop_right p (trim (char '}'))) st
*)

let parse_identifier eta =
  let ans = take_while1_string is_alphanum eta in
  match ans with
  | Prez_error e -> Prez_error e
  | Prez_success (name, pos1) ->
    if is_cpp_keyword name
    then Prez_error (Perr_message "keyword as identifier")
    else if is_digit (string_nth name 0)
    then Prez_error (Perr_expected "identifier")
    else Prez_success (name, pos1)
;;

let parse_type eta =
  choice
    [ bind (skip_ws (string "int")) (fun _ -> return Ptype_int)
    ; bind (skip_ws (string "void")) (fun _ -> return Ptype_void)
    ]
    eta
;;

let make_binop_level expr_parser ops =
  let make_parser (op_str, op_ctor) =
    drop_left
      (trim (string op_str))
      (map expr_parser (fun right left -> Pexpr_binop (op_ctor, left, right)))
  in
  let level = choice (list_map make_parser ops) in
  bind expr_parser (fun init ->
    map (many level) (fun fs -> list_fold (fun acc f -> f acc) fs init))
;;

let parse_expr_call parse_expr =
  bind (skip_ws parse_identifier) (fun fname ->
    parens
      (choice2
         (* at least one arg *)
         (bind parse_expr (fun e1 ->
            bind
              (many (skip_ws (drop_left (char ',') parse_expr)))
              (fun es -> return (Pexpr_call (fname, e1 :: es)))))
         (* no args *)
         (return (Pexpr_call (fname, [])))))
;;

let num_of_string_exn s =
  let rec loop s pos len acc =
    if pos < len
    then (
      let d = char_code (string_nth s pos) - char_code '0' in
      loop s (1 + pos) len ((acc * 10) + d))
    else acc
  in
  loop s 0 (string_len s) 0
;;

let lookahead pred pos =
  let s = sarr.(0) in
  let len = string_len s in
  if pos < len then pred (string_nth s pos) else false
;;

let comma_separated p pos =
  let rec helper p pos acc =
    let pos = ws_exn pos in
    match drop_left (char ',') p pos with
    | Prez_error e -> Prez_success (list_rev acc, pos)
    | Prez_success (next, pos) -> helper p pos (next :: acc)
  in
  bind
    p
    (fun h pos ->
       let pos = ws_exn pos in
       if lookahead is_comma pos then helper p pos [ h ] else return [ h ] pos)
    pos
;;

let mul_div_spec = [ "*", Pbinop_mul; "/", Pbinop_div ]
let add_sub_spec = [ "+", Pbinop_add; "-", Pbinop_sub ]

let comp_spec =
  [ "==", Pbinop_eq
  ; "!=", Pbinop_ne
  ; "<", Pbinop_lt
  ; ">", Pbinop_gt
  ; "<=", Pbinop_le
  ; ">=", Pbinop_ge
  ]
;;

let logical_spec = [ "&&", Pbinop_and; "||", Pbinop_or ]
let is_open_paren c = c = '('
let is_qm c = c = '?'

let rec parse_expr_atom parse_expr pos =
  let pos = ws_exn pos in
  if lookahead is_open_paren pos (* "(" <expr> ")" *)
  then parens parse_expr pos
  else if lookahead is_digit pos (* "(" <number> ")" *)
  then
    map (take_while1_string is_digit) (fun chs -> Pexpr_int (num_of_string_exn chs)) pos
  else (* application or identifier *)
    bind
      parse_identifier
      (fun fname pos ->
         let pos = ws_exn pos in
         if lookahead is_lparen pos
         then
           drop_left
             lparen
             (fun pos ->
                let pos = ws_exn pos in
                if lookahead is_rparen pos
                then drop_left rparen (return (Pexpr_call (fname, []))) pos
                else
                  bind
                    (drop_right (comma_separated parse_expr) (skip_ws rparen))
                    (fun es -> return (Pexpr_call (fname, es)))
                    pos)
             pos
         else return (Pexpr_var fname) pos)
      pos

and parse_expr_binop parse_expr =
  let parse_mul_div = make_binop_level (parse_expr_atom parse_expr) mul_div_spec in
  let parse_add_sub = make_binop_level parse_mul_div add_sub_spec in
  let parse_comp = make_binop_level parse_add_sub comp_spec in
  let parse_logical = make_binop_level parse_comp logical_spec in
  parse_logical

and parse_expr_tern parse_expr pos =
  let pos = ws_exn pos in
  bind
    (parse_expr_binop parse_expr)
    (fun c pos ->
       let pos = ws_exn pos in
       if lookahead is_qm pos
       then
         drop_left
           (char '?')
           (bind parse_expr (fun t ->
              drop_left
                (skip_ws (char ':'))
                (bind parse_expr (fun e -> return (Pexpr_tern (c, t, e))))))
           pos
       else return c pos)
    pos

and parse_expr_assign pos =
  let pos = ws_exn pos in
  choice2
    (bind (skip_ws parse_identifier) (fun v ->
       drop_left
         (skip_ws (char '='))
         (bind parse_expression (fun rhs -> return (Pexpr_assign (v, rhs))))))
    (parse_expr_tern parse_expression)
    pos

and parse_expression state = parse_expr_assign state

(*
   let __lifted_lam_23 chs = Pexpr_int (num_of_string_exn chs)
let __lifted_lam_25 fname es = return (Pexpr_call (fname, es))

let __lifted_lam_24 parse_expr fname pos =
  let pos = ws_exn pos in
  if lookahead is_rparen pos
  then drop_left rparen (return (Pexpr_call (fname, []))) pos
  else
    bind
      (drop_right (comma_separated parse_expr) (skip_ws rparen))
      (__lifted_lam_25 fname)
      pos
;;

let __lifted_lam_22 parse_expr fname pos =
  let pos = ws_exn pos in
  if lookahead is_lparen pos
  then drop_left lparen (__lifted_lam_24 parse_expr fname) pos
  else return (Pexpr_var fname) pos
;;

let __lifted_lam_28 t c e = return (Pexpr_tern (c, t, e))

let __lifted_lam_27 parse_expr c t =
  drop_left (skip_ws (char ':')) (bind parse_expr (__lifted_lam_28 t c))
;;

let __lifted_lam_26 parse_expr c pos =
  let pos = ws_exn pos in
  if lookahead is_qm pos
  then drop_left (char '?') (bind parse_expr (__lifted_lam_27 parse_expr c)) pos
  else return c pos
;;

let __lifted_lam_30 v rhs = return (Pexpr_assign (v, rhs))

let rec parse_expr_atom parse_expr pos =
  let pos = ws_exn pos in
  if lookahead is_open_paren pos
  then parens parse_expr pos
  else if lookahead is_digit pos
  then map (take_while1_string is_digit) __lifted_lam_23 pos
  else bind parse_identifier (__lifted_lam_22 parse_expr) pos

and parse_expr_binop parse_expr =
  let parse_mul_div = make_binop_level (parse_expr_atom parse_expr) mul_div_spec in
  let parse_add_sub = make_binop_level parse_mul_div add_sub_spec in
  let parse_comp = make_binop_level parse_add_sub comp_spec in
  let parse_logical = make_binop_level parse_comp logical_spec in
  parse_logical

and parse_expr_tern parse_expr pos =
  let pos = ws_exn pos in
  bind (parse_expr_binop parse_expr) (__lifted_lam_26 parse_expr) pos

and parse_expr_assign pos =
  let pos = ws_exn pos in
  choice2
    (bind (skip_ws parse_identifier) __lifted_lam_29)
    (parse_expr_tern parse_expression)
    pos

and parse_expression state = parse_expr_assign state

and __lifted_lam_29 v =
  drop_left (skip_ws (char '=')) (bind parse_expression (__lifted_lam_30 v))
;;
*)
let main = 1
