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

  let make () = !pos
  let rollback dest = pos := dest
end

let ws () =
  while !pos < !length && !text.[!pos] = ' ' do
    incr pos
  done

let is_alpha ch =
  let code = Char.code ch in
  Char.code 'a' <= code && code <= Char.code 'z'

let is_digit ch =
  let code = Char.code ch in
  Char.code '0' <= code && code <= Char.code '9'

let char c =
  if !pos < !length && !text.[!pos] = c then (
    incr pos;
    true)
  else false

let lookahead_cond cond =
  let curpos = ref !pos in
  while !curpos < !length && cond !text.[!pos] do
    incr curpos
  done;
  !curpos < !length && cond !text.[!curpos]

let lookahead_paren () =
  let curpos = ref !pos in
  while !curpos < !length && is_digit !text.[!pos] do
    incr curpos
  done;
  !curpos < !length && !text.[!curpos] = '('

module String_set = Set.Make (String)

let keywords =
  let open String_set in
  empty |> add "if" |> add "else" |> add "fi" |> add "then" |> add "do"
  |> add "while" |> add "done"

let is_keyword s = String_set.mem s keywords

let econst () =
  (* ws (); *)
  let acc = Buffer.create 5 in
  while !pos < !length && is_digit !text.[!pos] do
    Buffer.add_char acc !text.[!pos];
    incr pos
  done;
  if Buffer.length acc > 0 then
    Option.some @@ EConst (int_of_string (Buffer.contents acc))
  else None

let ident_or_keyword () =
  ws ();
  let acc = Buffer.create 5 in
  (* log "eident: pos = %d" !pos; *)
  while !pos < !length && is_alpha !text.[!pos] do
    Buffer.add_char acc !text.[!pos];
    incr pos
  done;
  if Buffer.length acc > 0 then
    let s = Buffer.contents acc in
    Some s
  else None

let ident () =
  match ident_or_keyword () with Some s when is_keyword s -> None | x -> x

let keyword kw =
  let exception Fail in
  ws ();
  let kwlen = String.length kw in
  let rec loop i =
    if i >= kwlen then ()
    else if !pos + i < !length && !text.[!pos + i] = kw.[i] then loop (i + 1)
    else raise Fail
  in
  match loop 0 with
  | exception Fail -> false
  | () ->
      if !pos + kwlen >= !length then (
        pos := !pos + kwlen;
        true)
      else if not (is_alpha !text.[!pos + kwlen]) then (
        pos := !pos + kwlen;
        true)
      else
        let () = log "parsing keyword %S failed" kw in
        false

let eident () =
  match ident () with
  | Some x when not (String_set.mem x keywords) -> Some (EVar x)
  | _ -> None

let rec expr_plus () =
  let ch, op = ('+', "+") in
  match expr_mul () with
  | None -> None
  | Some head ->
      (* log "got a head: %S" ([%show: AST.expr] head); *)
      if lookahead_paren () then
        let b1 : bool = char '(' in
        let rez = expr_plus () in
        let b2 : bool = char ')' in
        match rez with Some x when b1 && b2 -> Some x | _ -> None
      else
        let acc = ref head in
        let rec loop () =
          let rb1 = Rollback.make () in
          if char ch then (
            match expr_mul () with
            | None -> Rollback.rollback rb1
            | Some v ->
                acc := EBinop (op, !acc, v);
                loop ())
          else Rollback.rollback rb1
        in
        let () = loop () in
        Some !acc

and expr_mul () =
  let ch, op = ('*', "*") in
  match primary () with
  | None -> None
  | Some head ->
      (* log "got a head: %S" ([%show: AST.expr] head); *)
      let acc = ref head in
      let rec loop () =
        let rb1 = Rollback.make () in
        if char ch then (
          match eident () with
          | None -> Rollback.rollback rb1
          | Some v ->
              acc := EBinop (op, !acc, v);
              loop ())
        else Rollback.rollback rb1
      in
      loop ();
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
    | Some x ->
        (* log "ident %S parsed in primary" ([%show: AST.expr] x); *)
        Some x
    | None -> ( match econst () with None -> None | Some x -> Some x)

let expr = expr_plus

let rec statement () =
  let ( >>= ) = Option.bind in
  let rb1 = Rollback.make () in

  let rollback () =
    let () = Rollback.rollback rb1 in
    None
  in

  match ident_or_keyword () with
  | Some "fi" | None -> rollback ()
  | Some "while" ->
      ws ();
      expr () >>= fun econd ->
      if keyword "do" then (
        statements () >>= fun ebody ->
        (* log "%s %d. pos = %d" __FILE__ __LINE__ !pos; *)
        ws ();
        match ident_or_keyword () with
        | None -> rollback ()
        | Some "done" -> Some (While (econd, ebody))
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
        | Some "fi" ->
            (* log "%s %d. pos = %d" __FILE__ __LINE__ !pos; *)
            Some (Ite (econd, ethen, []))
        | Some "else" ->
            (* log "%s %d. pos = %d" __FILE__ __LINE__ !pos; *)
            statements () >>= fun eelse ->
            if keyword "fi" then Some (Ite (econd, ethen, eelse)) else None
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
    | Some v -> loop (v :: acc)
  in
  let stmts = loop [] in
  Some stmts

let parse_expr_string s =
  init s;
  expr ()

let parse_print_expr str =
  match parse_expr_string str with
  | Some ast -> Format.printf "%s\n%!" @@ [%show: AST.expr] ast
  | None -> print_endline "ERROR"

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

let%expect_test "just ident" =
  logoff ();
  Format.printf "%s\n%!" @@ [%show: AST.expr option] (parse_expr_string "a");
  [%expect {|
    (Some (EVar "a")) |}]

let%expect_test "just a constant" =
  logon ();
  parse_print_expr "42";
  [%expect {|
    (EConst 42) |}]

let%expect_test "a+b+c" =
  logoff ();
  parse_print_expr "a+b+c";
  [%expect
    {|
        (EBinop ("+", (EBinop ("+", (EVar "a"), (EVar "b"))), (EVar "c"))) |}]

let%expect_test " a" =
  logoff ();
  parse_print_expr " a";
  [%expect {|
        (EVar "a") |}]

let%expect_test "a+b*c" =
  logoff ();
  parse_print_expr "a+b*c";
  [%expect
    {|
    (EBinop ("+", (EVar "a"), (EBinop ("*", (EVar "b"), (EVar "c"))))) |}]

let%expect_test "b*c+a" =
  logoff ();
  parse_print_expr "b*c+a";
  [%expect
    {|
    (EBinop ("+", (EBinop ("*", (EVar "b"), (EVar "c"))), (EVar "a"))) |}]

let%expect_test "a+b*c+1" =
  logoff ();
  parse_print_expr "a+b*c+1";
  [%expect
    {|
    (EBinop ("+",
       (EBinop ("+", (EVar "a"), (EBinop ("*", (EVar "b"), (EVar "c"))))),
       (EConst 1))) |}]

let%expect_test "parens: (b)" =
  parse_print_expr "(b)";
  [%expect {|
    (EVar "b") |}]

let%expect_test "parens " =
  parse_print_expr "(b)+1";
  [%expect {|
    (EBinop ("+", (EVar "b"), (EConst 1))) |}]

let%expect_test "parens " =
  parse_print_expr "(b+a)*c";
  [%expect
    {|
    (EBinop ("*", (EBinop ("+", (EVar "b"), (EVar "a"))), (EVar "c"))) |}]

let%expect_test "program" =
  parse_print_program "x=5;";
  [%expect {|
    [(Assgn ("x", (EConst 5)))] |}]

let%test "keyword 'fi'" =
  init " fi";
  keyword "fi"

let%test "keyword 'if'" =
  init "if";
  keyword "if"

let%expect_test "stmt doesn't eat too much " =
  init " fi";
  (match statement () with
  | Some _ -> print_endline "failed"
  | None ->
      log "%s %d. pos = %d" __FILE__ __LINE__ !pos;
      Printf.printf "%b\n" (!pos < 3));
  [%expect "true"]

let%expect_test "  " =
  logon ();
  init " fi";
  (match statement () with
  | None -> print_endline "failed"
  | Some _ -> Printf.printf "%b" (keyword "fi"));

  [%expect {|
    failed |}]

let%test "keyword 'then' " =
  init " then ";
  keyword "then"

let%expect_test "program" =
  logon ();
  parse_print_stmt "if a then fi";
  [%expect {|
    (Ite ((EVar "a"), [], [])) |}]

let%expect_test "program 'while'" =
  logon ();
  parse_print_stmt "while a do x=5; done";
  [%expect
    {|
    (While ((EVar "a"), [(Assgn ("x", (EConst 5)))])) |}]

let%expect_test "program" =
  parse_print_stmt "x=1;";
  [%expect {|
    (Assgn ("x", (EConst 1))) |}];
  parse_print_stmt "if a then x=1; y=2; else z=3; fi";
  [%expect
    {|
    (Ite ((EVar "a"), [(Assgn ("x", (EConst 1))); (Assgn ("y", (EConst 2)))],
       [(Assgn ("z", (EConst 3)))])) |}]
