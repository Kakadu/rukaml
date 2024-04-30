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

  val bmrk : unit -> t
  val rollback : t -> unit
end = struct
  type t = int

  let bmrk () = !pos
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
  !curpos < !length && !text.[!curpos] = '('

let lookahead_paren () =
  let curpos = ref !pos in
  while !curpos < !length && is_digit !text.[!pos] do
    incr curpos
  done;
  !curpos < !length && !text.[!curpos] = '('

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

let ident () =
  ws ();
  let acc = Buffer.create 5 in
  log "eident: pos = %d" !pos;
  while !pos < !length && is_alpha !text.[!pos] do
    Buffer.add_char acc !text.[!pos];
    incr pos
  done;
  if Buffer.length acc > 0 then Some (Buffer.contents acc) else None

let eident () = match ident () with Some x -> Some (EVar x) | None -> None

let rec expr_plus () =
  let ch, op = ('+', "+") in
  match expr_mul () with
  | None -> None
  | Some head ->
      if lookahead_paren () then
        let b1 : bool = char '(' in
        let rez = expr_plus () in
        let b2 : bool = char ')' in
        match rez with Some x when b1 && b2 -> Some x | _ -> None
      else
        let acc = ref head in
        let rec loop () =
          let rb1 = Rollback.bmrk () in
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
      let acc = ref head in
      let rec loop () =
        let rb1 = Rollback.bmrk () in
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
  if lookahead_paren () then
    let b1 : bool = char '(' in
    let rez = expr_plus () in
    let b2 : bool = char ')' in
    if b1 && b2 then rez else None
  else
    match eident () with
    | Some x ->
        log "ident %S parsed in primary" ([%show: AST.expr] x);
        Some x
    | None -> ( match econst () with None -> None | Some x -> Some x)

let expr = expr_plus

let statement () =
  match ident () with
  | None -> None
  | Some "if" -> assert false
  | Some lhs -> (
      let is_eq = char '=' in
      let rhs = expr () in
      log "%s %d %s" __FILE__ __LINE__ @@ [%show: AST.expr option] rhs;

      let is_semi = char ';' in
      match rhs with
      | Some r when is_eq && is_semi -> Some (Assgn (lhs, r))
      | _ -> None)

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
(* if !pos >= !length then Some stmts else None *)

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
    eident: pos = 0
    (EConst 42) |}]

let%expect_test "a+b+c" =
  logoff ();
  parse_print_expr "a+b+c";
  [%expect
    {|
        (EBinop ("+", (EBinop ("+", (EVar "a"), (EVar "b"))), (EVar "c"))) |}]

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

let%expect_test "program" =
  parse_print_program "if (a) then x=5; fi";
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  "Assert_failure lib/parser.ml:150:17"
  Raised at Parser.statement in file "lib/parser.ml", line 150, characters 17-29
  Called from Parser.program.loop in file "lib/parser.ml", line 172, characters 10-22
  Called from Parser.program in file "lib/parser.ml", line 174, characters 14-21
  Called from Parser.parse_print_program in file "lib/parser.ml", line 181, characters 8-18
  Called from Parser.(fun) in file "lib/parser.ml", line 255, characters 2-43
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 262, characters 12-19 |}]
