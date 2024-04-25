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

let econst () =
  (* ws (); *)
  let acc = Buffer.create 5 in
  while is_digit !text.[!pos] do
    Buffer.add_char acc !text.[!pos];
    incr pos
  done;
  if Buffer.length acc > 0 then
    Option.some @@ EConst (int_of_string (Buffer.contents acc))
  else None

let eident () =
  (* ws (); *)
  let acc = Buffer.create 5 in
  log "eident: pos = %d" !pos;
  while !pos < !length && is_alpha !text.[!pos] do
    Buffer.add_char acc !text.[!pos];
    incr pos
  done;

  if Buffer.length acc > 0 then Some (EVar (Buffer.contents acc)) else None

let rec expr_plus () =
  let ch, op = ('+', "+") in
  match eident () with
  | None -> None
  | Some head ->
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
      loop ();
      Some !acc

and expr_mul () =
  let ch, op = ('*', "*") in
  match eident () with
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

let expr = expr_plus

let parse_expr_string s =
  init s;
  expr ()

let parse_print_expr str =
  Format.printf "%s\n%!" @@ [%show: AST.expr option] (parse_expr_string str)

let parse ~filename =
  text := In_channel.with_open_text filename In_channel.input_all;
  init !text;
  ()

let%expect_test "just ident" =
  logoff ();
  Format.printf "%s\n%!" @@ [%show: AST.expr option] (parse_expr_string "a");
  [%expect {|
    (Some (EVar "a")) |}]

let%expect_test "a+b+c" =
  logoff ();
  parse_print_expr "a+b+c";
  [%expect
    {|
        (Some (EBinop ("+", (EBinop ("+", (EVar "a"), (EVar "b"))), (EVar "c")))) |}]

let%expect_test "a+b*c" =
  logoff ();
  parse_print_expr "a+b*c";
  [%expect
    {|
    (Some (EBinop ("+", (EVar "a"), (EBinop ("*", (EVar "b"), (EVar "c")))))) |}]

let%expect_test "a+b*c" =
  logoff ();
  parse_print_expr "a+b*c+d";
  [%expect
    {|
  (Some (EBinop ("+",
           (EBinop ("+", (EVar "a"), (EBinop ("*", (EVar "b"), (EVar "c"))))),
           (EVar "d")))) |}]
