(* [begin skip] *)

(* provides compability with ocamlopt (rukaml parser skips this block) *)

let string_of_char_list chs = String.of_seq (List.to_seq chs)
let string_len s = String.length s
let string_nth s n = s.[n]
let string_equal = String.equal
let char_code = Char.code
let printf, fprintf, sprintf = Stdlib.Printf.(printf, fprintf, sprintf)
let array_get = Array.get
let array_set = Array.set
let array_len = Array.length
let list_length = List.length
let sys_argv = Sys.argv

let end_of_input ic =
  match input_char ic with
  | exception End_of_file -> true
  | c ->
    let () = seek_in ic (pos_in ic - 1) in
    false
;;

(* [end skip] *)

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

let is_lowercase ch = char_code 'a' <= char_code ch && char_code ch <= char_code 'z'
let is_uppercase ch = char_code 'A' <= char_code ch && char_code ch <= char_code 'Z'
let is_digit ch = char_code '0' <= char_code ch && char_code ch <= char_code '9'
let is_alphanum ch = is_lowercase ch || is_uppercase ch || is_digit ch || ch = '_'
let int_of_digit ch = char_code ch - char_code '0'

let int_of_digits chs =
  let rec pow b e = if e < 1 then 1 else b * pow b (e - 1) in
  let rez, _ =
    list_fold_right
      (fun digit (acc, pos) -> acc + (pow 10 pos * int_of_digit digit), pos + 1)
      chs
      (0, 0)
  in
  rez
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

(* ast printer *)

let pp_option pp_item oc t =
  match t with
  | None -> ()
  | Some item -> pp_item oc item
;;

let pp_list pp_item sep oc t =
  match t with
  | [] -> ()
  | x1 :: xs ->
    let rec loop oc ls =
      match ls with
      | [] -> ()
      | x1 :: xs ->
        let () = fprintf oc "%s%a" sep pp_item x1 in
        loop oc xs
    in
    fprintf oc "%a%s%a" pp_item x1 sep loop xs
;;

let pp_ptype oc t =
  match t with
  | Ptype_int -> fprintf oc "int"
  | Ptype_void -> fprintf oc "void"
;;

let pp_pbinop oc op =
  match op with
  | Pbinop_add -> fprintf oc "+"
  | Pbinop_sub -> fprintf oc "-"
  | Pbinop_mul -> fprintf oc "*"
  | Pbinop_div -> fprintf oc "/"
  | Pbinop_eq -> fprintf oc "=="
  | Pbinop_ne -> fprintf oc "!="
  | Pbinop_lt -> fprintf oc "<"
  | Pbinop_gt -> fprintf oc ">"
  | Pbinop_le -> fprintf oc "<="
  | Pbinop_ge -> fprintf oc ">="
  | Pbinop_and -> fprintf oc "&&"
  | Pbinop_or -> fprintf oc "||"
;;

let rec pp_pexpr oc e =
  match e with
  | Pexpr_int n -> fprintf oc "%d" n
  | Pexpr_var s -> fprintf oc "%s" s
  | Pexpr_binop (op, e1, e2) ->
    fprintf oc "(%a %a %a)" pp_pexpr e1 pp_pbinop op pp_pexpr e2
  | Pexpr_call (f, args) ->
    let () = fprintf oc "%s(" f in
    let rec pp_args xs =
      match xs with
      | [] -> ()
      | [ a ] -> pp_pexpr oc a
      | a :: rest ->
        let () = fprintf oc "%a, " pp_pexpr a in
        pp_args rest
    in
    let () = pp_args args in
    fprintf oc ")"
  | Pexpr_assign (v, e) -> fprintf oc "%s = %a" v pp_pexpr e
  | Pexpr_tern (c, t, e) -> fprintf oc "%a ? %a : %a" pp_pexpr c pp_pexpr t pp_pexpr e
;;

let pp_decl oc decl =
  let pp_vb oc (name, expr_opt) =
    match expr_opt with
    | None -> fprintf oc "%s" name
    | Some expr -> fprintf oc "%s = %a" name pp_pexpr expr
  in
  match decl with
  | Pdecl_var (ty, vbs) ->
    let () = fprintf oc "%a " pp_ptype ty in
    (match vbs with
     | [] -> failwith "should not happen"
     | [ vb ] -> pp_vb oc vb
     | vb1 :: vbs ->
       let () = pp_vb oc vb1 in
       fprintf oc "%a" (pp_list pp_vb ", ") vbs)
;;

let rec pp_pstmt oc stmt =
  match stmt with
  | Pstmt_return None -> fprintf oc "return;"
  | Pstmt_return (Some e) -> fprintf oc "return %a;" pp_pexpr e
  | Pstmt_expr e -> fprintf oc "%a;" pp_pexpr e
  | Pstmt_block stmts ->
    let () = fprintf oc "{\n" in
    let () = list_iter (fun s -> fprintf oc "  %a\n" pp_pstmt s) stmts in
    fprintf oc "}"
  | Pstmt_decl decl -> fprintf oc "%a;\n" pp_decl decl
  | Pstmt_ite (c, t, None) -> fprintf oc "if (%a) %a\n" pp_pexpr c pp_pstmt t
  | Pstmt_ite (c, t, Some e) ->
    let () = fprintf oc "if (%a) %a\n" pp_pexpr c pp_pstmt t in
    fprintf oc "else %a\n" pp_pstmt e
  | Pstmt_for (decl_opt, cond_opt, iter_opt, body_opt) ->
    let () =
      fprintf
        oc
        "for (%a;%a;%a)"
        (pp_option pp_decl)
        decl_opt
        (pp_option pp_pexpr)
        cond_opt
        (pp_option pp_pexpr)
        iter_opt
    in
    (match body_opt with
     | Some body ->
       let () = fprintf oc "\n" in
       pp_pstmt oc body
     | None -> fprintf oc ";\n")
;;

let pp_pprogram_item oc func =
  match func with
  | Pitem_function (fname, ftype, fparams, fbody) ->
    let () = fprintf oc "%a %s(" pp_ptype ftype fname in
    let rec pp_params xs =
      match xs with
      | [] -> ()
      | [ (t, n) ] -> fprintf oc "%a %s" pp_ptype t n
      | (t, n) :: rest ->
        let () = fprintf oc "%a %s, " pp_ptype t n in
        pp_params rest
    in
    let () = pp_params fparams in
    let () = fprintf oc ") {\n" in
    let () = list_iter (fprintf oc "  %a\n" pp_pstmt) fbody in
    fprintf oc "}\n"
;;

let pp_pprogram oc prog = list_iter (pp_pprogram_item oc) prog

(* parser combinators *)

type parsing_error =
  | Perr_message of string
  | Perr_expected of string
  | Perr_unexpected_eof

type parser_state = string * int

type 'a parsing_result =
  | Prez_success of 'a * parser_state
  | Prez_error of parsing_error

type 'a parser = parser_state -> 'a parsing_result

let return x state = Prez_success (x, state)
let fail err _state = Prez_error err

let bind p f state =
  (* let (_,pos) = state in *)
  (* let () = printf "calling bind on pos %d\n" pos in *)

  match p state with
  | Prez_error err -> Prez_error err
  | Prez_success (x, state2) ->
      f x state2
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
        (* let () = printf "Many finishes successfully\n" in *)

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

let skip_while pred state =
  let rec aux state =
    match state with
    | str, pos ->
      if pos >= string_len str
      then return () (str, pos)
      else (
        let ch = string_nth str pos in
        if pred ch then aux (str, pos + 1) else return () (str, pos))
  in
  aux state
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
(*
  let (_,pos) = state in
  let () = printf "calling drop_left on pos %d\n" pos in*)
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
  else
    let ch0 = string_nth str pos in
    (* let() = printf "Trying to parse '%c' at pos %d = '%c'\n" ch pos ch0 in *)
    if ch0 = ch
  then return ch (str, pos + 1)
  else Prez_error (Perr_message "unexpected char")
;;

let string expected (str, pos) =

  let rec aux explen len i =
    if i >= explen
    then
      return expected (str, pos + i)
    else if pos + i >= len
    then
      Prez_error Perr_unexpected_eof
    else
      let c1 = string_nth str (pos + i) in
      let c2 = string_nth expected i in
      if c1 = c2
    then aux explen len (i + 1)
    else
      Prez_error (Perr_message "unexpected string")
  in
  aux (string_len expected) (string_len str) 0
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

let ws eta =
  let rec helper len s pos =
    if pos >= len
    then Prez_success ((), (s, pos))
    else
      let ch = string_nth s pos in
      let cond = (ch = ' ') || (ch = '\n') in
      if cond
      then helper len s (pos+1)
      else
        Prez_success ((), (s, pos))
  in
  let (str, pos) = eta in
  helper (string_len str) str pos
;;

let skip_ws p state =

  (* let (_,pos) = state in
  let () = printf "calling skip_ws on pos %d\n" pos in *)
  drop_left ws p state

let trim p = drop_right (drop_left ws p) ws
let parens p = drop_left (trim (char '(')) (drop_right p (trim (char ')')))
let braces p = drop_left (trim (char '{')) (drop_right p (trim (char '}')))

let parse_identifier eta =
  bind (take_while1 is_alphanum) (fun chs ->
    let l = list_length chs in
    (* let() = printf "list len = %d\n" l in *)
    let name = string_of_char_list chs in
    if is_cpp_keyword name
    then fail (Perr_message "keyword as identifier")
    else if is_digit (string_nth name 0)
    then fail (Perr_expected "identifier")
    else return name) eta
;;

let parse_type eta =
  (*
  let (_,pos) = eta in
  let () = printf "calling parse_type on pos %d\n" pos in
  *)
  choice
    [ drop_left (string "int") (return Ptype_int)
    ; drop_left (string "void") (return Ptype_void)
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

let parse_expr_atom parse_expr =
  skip_ws
    (choice
       [ parse_expr_call parse_expr
       ; map (take_while1 is_digit) (fun chs -> Pexpr_int (int_of_digits chs))
       ; map parse_identifier (fun name -> Pexpr_var name)
       ; parens parse_expr
       ])
;;

let parse_expr_binop parse_expr =
  let parse_mul_div =
    make_binop_level (parse_expr_atom parse_expr) [ "*", Pbinop_mul; "/", Pbinop_div ]
  in
  let parse_add_sub =
    make_binop_level parse_mul_div [ "+", Pbinop_add; "-", Pbinop_sub ]
  in
  let parse_comp =
    make_binop_level
      parse_add_sub
      [ "==", Pbinop_eq
      ; "!=", Pbinop_ne
      ; "<", Pbinop_lt
      ; ">", Pbinop_gt
      ; "<=", Pbinop_le
      ; ">=", Pbinop_ge
      ]
  in
  let parse_logical = make_binop_level parse_comp [ "&&", Pbinop_and; "||", Pbinop_or ] in
  parse_logical
;;

let parse_expr_tern parse_expr =
  bind (parse_expr_binop parse_expr) (fun c ->
    choice2
      (drop_left
         (skip_ws (char '?'))
         (bind parse_expr (fun t ->
            drop_left
              (skip_ws (char ':'))
              (bind parse_expr (fun e -> return (Pexpr_tern (c, t, e)))))))
      (return c))
;;

let parse_expr_assign parse_expr =
  choice2
    (bind (skip_ws parse_identifier) (fun v ->
       drop_left
         (skip_ws (char '='))
         (bind parse_expr (fun rhs -> return (Pexpr_assign (v, rhs))))))
    (parse_expr_tern parse_expr)
;;

let rec parse_expression state = parse_expr_assign parse_expression state

let parse_decl =
  let parse_single_var =
    bind (skip_ws parse_identifier) (fun name ->
      choice2
        (drop_left
           (skip_ws (char '='))
           (map parse_expression (fun expr -> name, Some expr)))
        (return (name, None)))
  in
  bind (skip_ws parse_type) (fun t ->
    bind parse_single_var (fun v1 ->
      map
        (many (drop_left (skip_ws (char ',')) parse_single_var))
        (fun vs -> Pdecl_var (t, v1 :: vs))))
;;

let parse_stmt_return eta =
  drop_left
    (skip_ws (string "return"))
    (choice2
       (bind parse_expression (fun e ->
          drop_left (skip_ws (char ';')) (return (Pstmt_return (Some e)))))
       (skip_ws (drop_left (char ';') (return (Pstmt_return None)))))
       eta
;;

let parse_stmt_ite parse_stmt =
  drop_left
    (skip_ws (string "if"))
    (bind (parens parse_expression) (fun c ->
       bind parse_stmt (fun t ->
         choice2
           (drop_left
              (skip_ws (string "else"))
              (bind parse_stmt (fun e -> return (Pstmt_ite (c, t, Some e)))))
           (return (Pstmt_ite (c, t, None))))))
;;

let parse_stmt_for parse_stmt =
  let option p = choice2 (map p (fun x -> Some x)) (return None) in
  drop_left
    (skip_ws (string "for"))
    (drop_left
       (skip_ws (char '('))
       (bind (option parse_decl) (fun decl_opt ->
          drop_left
            (skip_ws (char ';'))
            (bind (option parse_expression) (fun cond_opt ->
               drop_left
                 (skip_ws (char ';'))
                 (bind
                    (option (parse_expr_assign parse_expression))
                    (fun iter_opt ->
                       drop_left
                         (skip_ws (char ')'))
                         (map
                            (choice2
                               (map parse_stmt (fun body -> Some body))
                               (drop_left (skip_ws (char ';')) (return None)))
                            (fun body_opt ->
                               Pstmt_for (decl_opt, cond_opt, iter_opt, body_opt))))))))))
;;

let parse_stmt_block parse_stmt =
  map (braces (many parse_stmt)) (fun stmts -> Pstmt_block stmts)
;;

let parse_stmt_decl =
  bind parse_decl (fun decl -> drop_left (skip_ws (char ';')) (return (Pstmt_decl decl)))
;;

let parse_stmt_expr eta =
  bind parse_expression (fun e -> drop_left (skip_ws (char ';')) (return (Pstmt_expr e))) eta
;;

let rec parse_statement state =
  (choice
     [ parse_stmt_return
     ; parse_stmt_decl
     ; parse_stmt_block parse_statement
     ; parse_stmt_ite parse_statement
     ; parse_stmt_for parse_statement
     ; parse_stmt_expr
     ])
    state
;;

let parse_fun_param eta =
  bind (skip_ws parse_type) (fun ty ->
    bind (skip_ws parse_identifier) (fun name -> return (ty, name))) eta
;;

let parse_fun_params  eta =
  parens
    (choice2
       (* at least one param *)
       (bind parse_fun_param (fun p1 ->
          bind
            (many (skip_ws (drop_left (char ',') parse_fun_param)))
            (fun ps -> return (p1 :: ps))))
       (* no params *)
       (return []))
        eta
;;

let parse_function eta =
  bind (skip_ws parse_type) (fun ty ->
    bind (skip_ws parse_identifier) (fun name ->
      bind parse_fun_params (fun params ->
        bind parse_statement (fun body ->
          match body with
          | Pstmt_block stmts -> return (Pitem_function (name, ty, params, stmts))
          | _ -> fail (Perr_expected "function body"))))) eta
;;

let parse_program eta =
  many1 (skip_ws parse_function) eta

let pp_parsing_error oc err =
  match err with
  | Perr_message msg -> fprintf oc "error: %s" msg
  | Perr_expected x -> fprintf oc "expected: %s" x
  | Perr_unexpected_eof -> fprintf oc "unexpected eof"
;;

(* codegen *)

let sizeof_ctype ptype =
  match ptype with
  | Ptype_int -> 8
  | Ptype_void -> 0
;;

(* C0 like implementation (every var gets it's own place in vars pool) *)
let eval_locals_pool_size stms =
  let eval_opt eval opt =
    match opt with
    | None -> 0
    | Some x -> eval x
  in
  let eval_decl decl =
    match decl with
    | Pdecl_var (ty, vars) -> sizeof_ctype ty * list_length vars
  in
  let rec aux stm =
    match stm with
    | Pstmt_decl decl -> eval_decl decl
    | Pstmt_ite (_, th, el_opt) -> aux th + eval_opt aux el_opt
    | Pstmt_for (Some decl, _, _, body_opt) -> eval_decl decl + eval_opt aux body_opt
    | Pstmt_for (None, _, _, body_opt) -> eval_opt aux body_opt
    | Pstmt_block stms -> list_fold (fun acc stm -> acc + aux stm) stms 0
    | Pstmt_return _ -> 0
    | Pstmt_expr _ -> 0
  in
  list_fold (fun acc stm -> acc + aux stm) stms 0
;;

let locals_pools = [| [] |]

let push_pool size =
  let old_pools = array_get locals_pools 0 in
  array_set locals_pools 0 ((0, size) :: old_pools)
;;

let pop_pool size =
  match array_get locals_pools 0 with
  | [] -> failwith "something bad: can not pop pool (no pools)"
  | (offset, _size) :: other_pools ->
    if size = _size
    then array_set locals_pools 0 other_pools
    else failwith "something bad: can not pop pool (size mismatch)"
;;

let emit_init_locals_pool oc pool_size =
  let () = push_pool pool_size in
  fprintf oc "addi sp, sp, %d\n" (pool_size * (0 - 1))
;;

let emit_destroy_locals_pool oc pool_size =
  let () = pop_pool pool_size in
  fprintf oc "addi sp, sp, %d\n" pool_size
;;

(* negative offsets over fp (use it for local vars, not for function parameters) *)
let get_offset_for_new_var nbytes =
  match array_get locals_pools 0 with
  | [] -> failwith "something bad: can not access pool (no pools)"
  | (offset, pool_size) :: other_pools ->
    let () =
      if offset + nbytes > pool_size
      then failwith "something bad: can not add var to pool (no enough space)"
      else array_set locals_pools 0 ((offset + nbytes, pool_size) :: other_pools)
    in
    0 - nbytes - offset
;;

(* scopes *)

let scopes = [| [] |]

let enter_scope () =
  let old_scopes = array_get scopes 0 in
  array_set scopes 0 ([] :: old_scopes)
;;

let exit_scope () =
  match array_get scopes 0 with
  | [] -> failwith "something bad: can not exit scope (no scopes)"
  | _ :: old_scopes -> array_set scopes 0 old_scopes
;;

let add_to_scope (name, offset) =
  match array_get scopes 0 with
  | [] -> failwith "something bad: can not add var (no scopes)"
  | current_scope :: old_scopes ->
    array_set scopes 0 (((name, offset) :: current_scope) :: old_scopes)
;;

let add_func_params_to_scope params =
  let rec aux params i =
    match params with
    | [] -> ()
    | (ty, name) :: xs ->
      let offset = 16 + (i * 8) in
      let () = add_to_scope (name, offset) in
      aux xs (i + 1)
  in
  aux params 0
;;

let find_var_offset name =
  let rec aux ss =
    match ss with
    | [] -> failwith (sprintf "something bad: can not find var %s offset" name)
    | [] :: ss -> aux ss
    | ((v, offset) :: vs) :: ss -> if v = name then offset else aux (vs :: ss)
  in
  aux (array_get scopes 0)
;;

let pp_local_var oc name =
  let offset = find_var_offset name in
  fprintf oc "%d(fp)" offset
;;

(* unique labels *)

let label_counter = [| 0 |] (* TODO: replace with ref *)

let fresh_label prefix =
  let n = array_get label_counter 0 in
  let () = array_set label_counter 0 (n + 1) in
  sprintf "%s_%d" prefix n
;;

(* codegen *)

let rec codegen_expr oc expr =
  match expr with
  | Pexpr_var name -> fprintf oc "  ld a0, %a\n" pp_local_var name
  | Pexpr_int n -> fprintf oc "  li a0, %d\n" n
  | Pexpr_tern (cond, t, e) ->
    let else_label = fresh_label ".Lelse" in
    let end_label = fresh_label ".Lend" in
    let () = codegen_expr oc cond in
    let () = fprintf oc "  beqz a0, %s\n" else_label in
    let () = codegen_expr oc t in
    let () = fprintf oc "  j %s\n" end_label in
    let () = fprintf oc "%s:\n" else_label in
    let () = codegen_expr oc e in
    let () = fprintf oc "%s:\n" end_label in
    ()
  | Pexpr_binop (op, e1, e2) ->
    let () = codegen_expr oc e1 in
    let () = fprintf oc "  addi sp, sp, -8\n" in
    let () = fprintf oc "  sd a0, 0(sp)\n" in
    let () = codegen_expr oc e2 in
    let () = fprintf oc "  ld t0, 0(sp)\n" in
    let () = fprintf oc "  addi sp, sp, 8\n" in
    (match op with
     | Pbinop_add -> fprintf oc "  add a0, t0, a0\n"
     | Pbinop_sub -> fprintf oc "  sub a0, t0, a0\n"
     | Pbinop_mul -> fprintf oc "  mul a0, t0, a0\n"
     | Pbinop_div -> fprintf oc "  div a0, t0, a0\n"
     | Pbinop_eq -> fprintf oc "  sub a0, t0, a0\n  seqz a0, a0\n"
     | Pbinop_ne -> fprintf oc "  sub a0, t0, a0\n  snez a0, a0\n"
     | Pbinop_lt -> fprintf oc "  slt a0, t0, a0\n"
     | Pbinop_gt -> fprintf oc "  slt a0, a0, t0\n"
     | Pbinop_le -> fprintf oc "  slt a0, a0, t0\n  xori a0, a0, 1\n"
     | Pbinop_ge -> fprintf oc "  slt a0, t0, a0\n  xori a0, a0, 1\n"
     | Pbinop_and -> fprintf oc "  and a0, t0, a0\n"
     | Pbinop_or -> fprintf oc "  or a0, t0, a0\n")
  | Pexpr_call (fname, args) ->
    let argc = list_length args in
    let rec alloc_args args i =
      match args with
      | [] -> ()
      | x :: xs ->
        let () = codegen_expr oc x in
        let () = fprintf oc "  sd a0, %d(sp)\n" (8 * (argc - i - 1)) in
        alloc_args xs (i + 1)
    in
    let () = if argc > 0 then fprintf oc "  addi sp, sp, -%d\n" (8 * argc) else () in
    let () = alloc_args args 0 in
    let () = fprintf oc "  call %s\n" fname in
    let () = if argc > 0 then fprintf oc "  addi sp, sp, %d\n" (8 * argc) else () in
    ()
  | Pexpr_assign (var, expr) ->
    let () = codegen_expr oc expr in
    fprintf oc "  sd a0, %a\n" pp_local_var var
;;

let codegen_decl oc decl =
  match decl with
  | Pdecl_var (ty, vbs) ->
    let tysize = sizeof_ctype ty in
    let rec aux vbs =
      match vbs with
      | [] -> ()
      | (name, rhs_opt) :: vbs ->
        let offset = get_offset_for_new_var tysize in
        let () = add_to_scope (name, offset) in
        (match rhs_opt with
         | Some expr ->
           let () = codegen_expr oc expr in
           fprintf oc "  sd a0, %a\n" pp_local_var name
         | None -> ())
    in
    aux vbs
;;

let rec codegen_statement oc epilogue stmt =
  match stmt with
  | Pstmt_return None -> fprintf oc "  j %s\n" epilogue
  | Pstmt_return (Some e) ->
    let () = codegen_expr oc e in
    let () = fprintf oc "  j %s\n" epilogue in
    ()
  | Pstmt_expr e -> codegen_expr oc e
  | Pstmt_block stmts -> list_iter (codegen_statement oc epilogue) stmts
  | Pstmt_ite (c, t, None) ->
    let () = codegen_expr oc c in
    let end_if_label = fresh_label "End_if" in
    let () = fprintf oc "  beqz a0, %s\n" end_if_label in
    let () = codegen_statement oc epilogue t in
    let () = fprintf oc "%s: \n" end_if_label in
    ()
  | Pstmt_ite (c, t, Some e) ->
    let () = codegen_expr oc c in
    let else_label = fresh_label "Else" in
    let end_if_label = fresh_label "End_if" in
    let () = fprintf oc "  beqz a0, %s\n" else_label in
    let () = codegen_statement oc epilogue t in
    let () = fprintf oc "  j %s\n" end_if_label in
    let () = fprintf oc "%s:\n" else_label in
    let () = codegen_statement oc epilogue e in
    let () = fprintf oc "%s:\n" end_if_label in
    ()
  | Pstmt_for (decl, cond_opt, iter_opt, body_opt) ->
    let call_option f x_opt =
      match x_opt with
      | None -> ()
      | Some x -> f x
    in
    let () = call_option (codegen_decl oc) decl in
    let loop_label = fresh_label "Loop" in
    let end_label = fresh_label "End_loop" in
    let () = fprintf oc "%s:\n" loop_label in
    let () = call_option (codegen_expr oc) cond_opt in
    let () = call_option (fun _ -> fprintf oc "  beqz a0, %s\n" end_label) cond_opt in
    let () = call_option (codegen_statement oc epilogue) body_opt in
    let () = call_option (codegen_expr oc) iter_opt in
    let () = fprintf oc "  j %s\n" loop_label in
    let () = fprintf oc "%s:\n" end_label in
    ()
  | Pstmt_decl decl -> codegen_decl oc decl
;;

let pp_prologue oc fname pool_size =
  let () = fprintf oc ".global %s\n" fname in
  let () = fprintf oc ".text\n" in
  let () = fprintf oc "%s:\n" fname in
  let () = fprintf oc "  addi sp, sp, -16\n" in
  let () = fprintf oc "  sd fp, 8(sp)\n" in
  let () = fprintf oc "  sd ra, 0(sp)\n" in
  let () = fprintf oc "  mv fp, sp\n" in
  let () = emit_init_locals_pool oc pool_size in
  ()
;;

let pp_epilogue oc epilogue_label pool_size =
  let () = fprintf oc "%s:\n" epilogue_label in
  let () = emit_destroy_locals_pool oc pool_size in
  let () = fprintf oc "  ld ra, 0(sp)\n" in
  let () = fprintf oc "  ld fp, 8(sp)\n" in
  let () = fprintf oc "  addi sp, sp, 16\n" in
  let () = fprintf oc "  ret\n" in
  ()
;;

let codegen_function oc func =
  match func with
  | Pitem_function (fname, _, params, stms) ->
    let () = enter_scope () in
    let () = add_func_params_to_scope params in
    let epilogue_label = fresh_label (sprintf "%s_epilogue" fname) in
    let pool_size = eval_locals_pool_size stms in
    let () = pp_prologue oc fname pool_size in
    let () = list_iter (codegen_statement oc epilogue_label) stms in
    let () = pp_epilogue oc epilogue_label pool_size in
    let () = exit_scope () in
    ()
;;

let codegen_program oc prog = list_iter (codegen_function oc) prog

(* driver *)

type target =
  | Parsetree
  | RiscV64



let usage () =
  let () = fprintf stderr "[compiler] Invalid args\n" in
  let () =
    fprintf
      stderr
      "[compiler] Usage: compiler <input-file> [-o <output-file>] [--target rv64|parsetree]\n"
  in
  exit 1
;;

let current_line state =
  let str, pos = state in
  let rec loop pos cnt =
    if pos >= string_len str
    then cnt
    else if string_nth str pos = '\n'
    then loop (pos + 1) (cnt + 1)
    else loop (pos + 1) cnt
  in
  loop pos 1
;;

let run_single oc target input =
  let () = printf "\n" in
  match parse_program (input, 0) with
  | Prez_error err -> printf "parsing failed: %a" pp_parsing_error err
  | Prez_success (ast, (_, pos)) ->
    if pos = string_len input
    then (
      match target with
      | Parsetree -> pp_pprogram oc ast
      | RiscV64 -> codegen_program oc ast)
    else
      printf
        "parsing failed: can not parse many program items (failed at ~%d line)\n"
        (current_line (input, pos) + 1)
;;

let test ok_msg input f =
  match f (input,0) with
  | Prez_error _ -> printf "Failed '%s'\n" ok_msg
  | Prez_success (_,(_,pos)) -> printf "Success '%s' on pos %d\n" ok_msg pos

let test1 () = test "demo1" "{return 1;}" (parse_stmt_block parse_statement)

let test2 () = test "demo2" "int main() {return 1;}" parse_function

let test3 () =
  let input = "factrec1(){}" in
  match parse_identifier (input,0) with
  | Prez_error _ -> printf "Failed\n"
  | Prez_success (name,(_,pos)) -> printf "Success '%s' on pos %d\n" name pos

let test4 () =
  let input = "void factrec1(){ return 5; }" in
  match parse_function (input,0) with
  | Prez_error _ -> printf "Failed\n"
  | Prez_success (f, (_,pos)) ->
    let () = pp_pprogram stdout [f] in
    printf "Success 'test4' on pos %d\n" pos

let test5 () =
  let input = "factrec1(){}" in
  match take_while is_alphanum (input,0) with
  | Prez_error _ -> printf "Failed\n"
  | Prez_success (chs,(_,pos)) ->
      let l = list_length chs in
      let name = string_of_char_list chs in
      printf "Success '%s' on pos %d\n" name pos

let parse_args argv =
  let argc = array_len argv in
  let rec loop i input_opt output_opt target_opt =
    if i >= argc
    then input_opt, output_opt, target_opt
    else (
      (* TODO: rewrite using match with *)
      let arg = array_get argv i in

      if string_equal arg "-o"
      then
        if i + 1 >= argc
        then usage ()
        else (
          match output_opt with
          | None -> loop (i + 2) input_opt (Some (array_get argv (i + 1))) target_opt
          | Some _ -> usage ())
      else if string_equal arg "--target"
      then
        if i + 1 >= argc
        then usage ()
        else (
          match target_opt with
          | None ->
            let target_str = array_get argv (i + 1) in
            if target_str = "rv64"
            then loop (i + 2) input_opt output_opt (Some RiscV64)
            else if target_str = "parsetree"
            then loop (i + 2) input_opt output_opt (Some Parsetree)
            else usage ()
          | Some _ -> usage ())
      else (
        match input_opt with
        | None -> loop (i + 1) (Some arg) output_opt target_opt
        | Some _ -> usage ()))
  in
  let input_opt, output_opt, target_opt = loop 1 None None None in
  let input_path =
    match input_opt with
    | None -> usage ()
    | Some s -> s
  in
  let output_path =
    match output_opt with
    | None -> "a.s"
    | Some s -> s
  in
  let target =
    match target_opt with
    | None -> usage ()
    | Some s -> s
  in
  input_path, output_path, target
  (* ("program.c", "output.s", RiscV64) *)
;;

(* main *)

let main =
  (* let () = test1 () in *)
  (* let () = test2 () in *)
  (* let () = test3 () in *)
  (* let () = test4 () in
  let () = test5 () in *)

  let input_path, output_path, target = parse_args sys_argv in
  let ch = open_in "program.c" in
  let the_string = rukaml_input_all ch in
  let () = printf "%s\n" the_string in
  let () = trace_rukaml_val the_string in
  let () = run_single stdout target the_string in

  0
;;
