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

module Rollback
(* : sig
     type t

     val make : unit -> t
     val rollback : t -> unit
   end *) =
struct
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

let is_digit ch =
  let code = Char.code ch in
  Char.code '0' <= code && code <= Char.code '9'

let is_alpha_digit ch =
  let code = Char.code ch in
  (Char.code 'a' <= code && code <= Char.code 'z')
  || (Char.code '0' <= code && code <= Char.code '9')

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
  while !curpos < !length && is_ws !text.[!pos] do
    incr curpos
  done;
  !curpos < !length && !text.[!curpos] = '('

let keywords =
  let open String_set in
  empty |> add "let" |> add "in" |> add "rec" |> add "then" |> add "else"

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
  let oldpos = !pos in
  let acc = Buffer.create 5 in
  (* log "eident: pos = %d" !pos; *)
  if !pos < !length && is_alpha !text.[!pos] then (
    Buffer.add_char acc !text.[!pos];
    incr pos);

  if Buffer.length acc > 0 then
    while !pos < !length && is_alpha_digit !text.[!pos] do
      Buffer.add_char acc !text.[!pos];
      incr pos
    done;
  if Buffer.length acc > 0 then
    let s = Buffer.contents acc in
    Some s
  else (
    pos := oldpos;
    None)

let ident () =
  match ident_or_keyword () with Some s when is_keyword s -> None | x -> x

let keyword kw =
  let exception Fail in
  ws ();
  let kwlen = String.length kw in
  let rec loop i =
    if i >= kwlen then ()
    else if
      !pos + i < !length
      && String.unsafe_get !text (!pos + i) = String.unsafe_get kw i
    then loop (i + 1)
    else raise Fail
  in
  match loop 0 with
  | exception Fail -> false
  | () ->
      if !pos + kwlen >= !length then (
        pos := !pos + kwlen;
        true)
      else if not (is_alpha (String.unsafe_get !text (!pos + kwlen))) then (
        pos := !pos + kwlen;
        true)
      else (* let () = log "parsing keyword %S failed" kw in *)
        false

let eident () =
  match ident () with
  | Some x when not (String_set.mem x keywords) -> Some (EVar x)
  | _ -> None

let rec expr_plus () =
  let opers = [ ('+', "+"); ('-', "-") ] in
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
        let rec loop = function
          | [] -> ()
          | (ch, op) :: tl ->
              let rb1 = Rollback.make () in
              if char ch then (
                match expr_mul () with
                | None -> Rollback.rollback rb1
                | Some v ->
                    acc := EBinop (op, !acc, v);
                    loop opers)
              else loop tl
        in
        let () = loop opers in
        Some !acc

and expr_mul () =
  log "expr_mul on pos = %d" !pos;
  let pp_oper ppf = function
    | `Char c -> Format.fprintf ppf "`Char %c" c
    | `Kw s -> Format.fprintf ppf "`Kw %S" s
  in
  let opers =
    [ (`Char '*', "*"); (`Char '/', "/"); (`Kw "mod", "mod"); (`Char '>', ">") ]
  in
  match primary () with
  | None -> None
  | Some head ->
      let acc = ref head in
      let rec loop = function
        | [] -> ()
        | (text, op) :: tl ->
            log "Looping opers on pos %d, oper = %a" !pos pp_oper text;
            let rb1 = Rollback.make () in
            let do_oper () =
              match text with `Char c -> char c | `Kw kw -> keyword kw
            in
            if
              ws ();
              do_oper ()
            then
              match eident () with
              | Some v ->
                  acc := EBinop (op, !acc, v);
                  loop opers
              | None -> (
                  match econst () with
                  | None -> Rollback.rollback rb1
                  | Some c ->
                      acc := EBinop (op, !acc, c);
                      loop opers)
            else (
              Rollback.rollback rb1;
              loop tl)
      in
      loop opers;
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

let expr = expr_plus

let ( ** ) a b () =
  a ();
  b ()
