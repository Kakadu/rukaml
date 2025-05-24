let log_enabled = ref false
let logoff () = log_enabled := false
let logon () = log_enabled := true

let log ppf =
  if !log_enabled then Format.kasprintf print_endline ppf
  else Format.ifprintf Format.std_formatter ppf

let text = ref ""
let length = ref 0
let pos = ref 0

let init s =
  text := s;
  length := String.length s;
  pos := 0

open AST

module Rollback : sig
  type t

  val make : unit -> t
  val rollback : t -> unit
end = struct
  type t = int

  let make () = !pos [@@inline]
  let rollback dest = pos := dest [@@inline]
end

let is_ws = function '\n' | ' ' -> true | _ -> false

let ws () =
  while !pos < !length && is_ws !text.[!pos] do
    incr pos
  done

let is_alpha ch =
  let code = Char.code ch in
  Char.code 'a' <= code && code <= Char.code 'z'
[@@inline]

let is_digit ch =
  let code = Char.code ch in
  Char.code '0' <= code && code <= Char.code '9'
[@@inline]

let is_alpha_digit ch =
  let code = Char.code ch in
  (Char.code 'a' <= code && code <= Char.code 'z')
  || (Char.code '0' <= code && code <= Char.code '9')
[@@inline]

let char c =
  if !pos < !length && !text.[!pos] = c then (
    incr pos;
    true)
  else false
[@@inline]

let lookahead_cond cond =
  let curpos = ref !pos in
  while !curpos < !length && cond !text.[!pos] do
    incr curpos
  done;
  !curpos < !length && cond !text.[!curpos]

let lookahead_paren () =
  let curpos = ref !pos in
  while !curpos < !length && is_ws !text.[!pos] do
    incr curpos
  done;
  !curpos < !length && !text.[!curpos] = '('

let keywords =
  let open String_set in
  empty |> add "if" |> add "else" |> add "fi" |> add "then" |> add "do"
  |> add "while" |> add "done"

let is_keyword s = String_set.mem s keywords

let econst () =
  let acc = ref 0 in
  if !pos < !length && is_digit !text.[!pos] then (
    acc := Char.code !text.[!pos] - Char.code '0';
    incr pos;
    while !pos < !length && is_digit !text.[!pos] do
      acc := (!acc * 10) + Char.code !text.[!pos] - Char.code '0';
      incr pos
    done;
    Option.some @@ EConst !acc)
  else None

(** Parses identifiers without keyword check  *)
let ident_or_keyword () =
  ws ();
  let oldpos = !pos in
  let left = !pos in
  try
    let right0 =
      if !pos < !length && is_alpha !text.[!pos] then (
        incr pos;
        !pos)
      else raise_notrace Exit
    in

    let rec loop right =
      if !pos < !length && is_alpha_digit !text.[!pos] then (
        incr pos;
        loop (1 + right))
      else right
    in
    let right = loop right0 in
    if right > left then
      let s = StringLabels.sub !text ~pos:left ~len:(right - left) in
      Some s
    else raise_notrace Exit
  with Exit ->
    pos := oldpos;
    None

let ident () =
  match ident_or_keyword () with Some s when is_keyword s -> None | x -> x

let keyword kw =
  let exception Fail in
  ws ();
  let kwlen = String.length kw in
  let rec loop kwlen i =
    if i >= kwlen then ()
    else if !pos + i < !length && !text.[!pos + i] = kw.[i] then
      loop kwlen (i + 1)
    else raise_notrace Fail
  in
  match loop kwlen 0 with
  | exception Fail -> false
  | () ->
      if !pos + kwlen >= !length then (
        pos := !pos + kwlen;
        true)
      else if not (is_alpha !text.[!pos + kwlen]) then (
        pos := !pos + kwlen;
        true)
      else (* let () = log "parsing keyword %S failed" kw in *)
        false

let eident () =
  match ident () with
  | Some x when not (String_set.mem x keywords) -> Some (EVar x)
  | _ -> None

type oper_t = Char of char | Kw of string

let pp_oper ppf = function
  | Char c -> Format.fprintf ppf "`Char %c" c
  | Kw s -> Format.fprintf ppf "`Kw %S" s

let mul_opers =
  [ (Char '*', "*"); (Char '/', "/"); (Char '>', ">"); (Kw "mod", "mod") ]

let plus_opers = [ ('+', "+"); ('-', "-") ]

let rec expr_plus () =
  match expr_mul () with
  | None -> None
  | Some head ->
      (* log "got a head: %S" ([%show: AST.expr] head); *)
      let acc = ref head in
      let rec loop = function
        | [] -> ()
        | (ch, op) :: tl ->
            ws ();
            let rb1 = Rollback.make () in
            if char ch then (
              match expr_mul () with
              | None -> Rollback.rollback rb1
              | Some v ->
                  acc := EBinop (op, !acc, v);
                  loop plus_opers)
            else loop tl
      in
      let () = loop plus_opers in
      Some !acc

and expr_mul () =
  (* log "expr_mul on pos = %d" !pos; *)
  match primary_non_kw () with
  | None -> None
  | Some head ->
      let acc = ref head in
      let rec loop = function
        | [] -> ()
        | (text, op) :: tl ->
            (* log "Looping opers on pos %d, oper = %a" !pos pp_oper text; *)
            let rb1 = Rollback.make () in
            let do_oper text =
              match text with Char c -> char c | Kw kw -> keyword kw
            in
            if
              ws ();
              do_oper text
            then
              match eident () with
              | Some v ->
                  acc := EBinop (op, !acc, v);
                  loop mul_opers
              | None -> (
                  match econst () with
                  | Some c ->
                      acc := EBinop (op, !acc, c);
                      loop mul_opers
                  | None -> (
                      let b0 = char '(' in
                      let rez = expr_plus () in
                      let b2 = char ')' in
                      match (b0, rez, b2) with
                      | true, Some rez, true ->
                          acc := EBinop (op, !acc, rez);
                          loop mul_opers
                      | _ -> Rollback.rollback rb1))
            else (
              Rollback.rollback rb1;
              loop tl)
      in
      loop mul_opers;
      Some !acc

and primary () =
  (* log " %s %d , pos = %d" __FILE__ __LINE__ !pos; *)
  if lookahead_paren () then
    let b1 : bool = char '(' in
    let rez = expr_plus () in
    let b2 : bool = char ')' in
    if b1 && b2 then rez else None
  else
    match eident () with
    | Some x -> Some x
    | None -> ( match econst () with None -> None | Some x -> Some x)

and primary_non_kw () =
  (* log " %s %d , pos = %d" __FILE__ __LINE__ !pos; *)
  if lookahead_paren () then
    let b1 : bool = char '(' in
    let rez = expr_plus () in
    let b2 : bool = char ')' in
    if b1 && b2 then rez else None
  else
    match ident_or_keyword () with
    | Some x -> Some (EVar x)
    | None -> ( match econst () with None -> None | Some x -> Some x)

let expr = expr_plus

let ( ** ) a b () =
  a ();
  b ()

let rec statement () =
  let ( >>= ) = Option.bind in
  let rb1 = Rollback.make () in

  let rollback () =
    let () = Rollback.rollback rb1 in
    None
  in
  let and_semic_too rez =
    ws ();
    if char ';' then Some rez
    else
      let () = Rollback.rollback rb1 in
      None
  in
  match ident_or_keyword () with
  | Some "fi" | None -> rollback ()
  | Some "while" ->
      ws ();
      expr () >>= fun econd ->
      ws ();
      (* log "%s %d. pos = %d" __FILE__ __LINE__ !pos; *)
      if keyword "do" then (
        (* log "%s %d. pos = %d" __FILE__ __LINE__ !pos; *)
        statements ()
        >>= fun ebody ->
        (* log "%s %d. pos = %d" __FILE__ __LINE__ !pos; *)
        ws ();
        match ident_or_keyword () with
        | None -> rollback ()
        | Some "done" ->
            ws ();
            if char ';' then Some (While (econd, ebody)) else rollback ()
        | Some _ -> rollback ())
      else rollback ()
  | Some "if" ->
      ws ();
      expr () >>= fun econd ->
      (* log "%s %d. pos = %d" __FILE__ __LINE__ !pos; *)
      let () = ws () in
      (* log "%s %d. pos = %d" __FILE__ __LINE__ !pos; *)
      if keyword "then" then (
        (* log "%s %d isKWthen = %b" __FILE__ __LINE__ isKWthen; *)
        (* log "%s %d. Calling 'statements'... pos = %d" __FILE__ __LINE__ !pos; *)
        statements ()
        >>= fun ethen ->
        ws ();
        (* log "%s %d. ethen.length = %d" __FILE__ __LINE__ (List.length ethen); *)
        (* log "%s %d. pos = %d" __FILE__ __LINE__ !pos; *)
        match ident_or_keyword () with
        | None ->
            (* log "%s %d. pos = %d" __FILE__ __LINE__ !pos; *)
            let () = Rollback.rollback rb1 in
            None
        | Some "fi" -> and_semic_too (Ite (econd, ethen, []))
        | Some "else" ->
            (* log "%s %d. pos = %d" __FILE__ __LINE__ !pos; *)
            statements () >>= fun eelse ->
            if keyword "fi" then and_semic_too (Ite (econd, ethen, eelse))
            else None
        | Some _ -> rollback ())
      else rollback ()
  | Some kw when is_keyword kw -> rollback ()
  | Some lhs -> (
      let is_eq = char '=' in
      if not is_eq then rollback ()
      else
        match expr () with
        | None -> rollback ()
        | Some rhs ->
            (* log "%s %d %s" __FILE__ __LINE__ @@ [%show: AST.expr option] rhs; *)
            if char ';' then Some (Assgn (lhs, rhs)) else rollback ())

and statements () : _ list option =
  let rec loop acc =
    match statement () with
    | None ->
        (* log "%s %d. pos = %d" __FILE__ __LINE__ !pos; *)
        List.rev acc
    | Some v ->
        log "Statement eaten: %s\n" (show_stmt v);
        loop (v :: acc)
  in
  let stmts = loop [] in
  Some stmts

let parse_ident_string s =
  init s;
  ident ()

let parse_expr_string s =
  init s;
  expr ()

let parse_print_expr str =
  match parse_expr_string str with
  | Some ast -> Format.printf "%s\n%!" @@ [%show: AST.expr] ast
  | None -> print_endline "ERROR"

let parse_stmt_string s =
  init s;
  statement ()

let program () =
  let rec loop acc =
    match statement () with None -> List.rev acc | Some v -> loop (v :: acc)
  in
  let stmts = loop [] in
  ws ();
  Some stmts

let parse_print_stmt str =
  init str;
  match statement () with
  | Some ast -> Format.printf "%s\n%!" @@ [%show: AST.stmt] ast
  | None -> print_endline "ERROR"

let parse_print_program str =
  init str;
  match program () with
  | Some ast -> Format.printf "%s\n%!" @@ [%show: AST.stmt list] ast
  | None -> print_endline "ERROR"

let parse ~filename =
  text := In_channel.with_open_text filename In_channel.input_all;
  init !text;
  ()
