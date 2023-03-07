open Angstrom
open Parsetree

type cfg = { mutable use_logging : bool }

let cfg = { use_logging = false }
let set_logging flg = cfg.use_logging <- flg

let log fmt =
  if cfg.use_logging
  then Format.kasprintf (fun s -> Format.printf "%s\n%!" s) fmt
  else Format.ifprintf Format.std_formatter fmt
;;

let pp_list eta = Format.pp_print_list ~pp_sep:(fun ppf () -> Format.fprintf ppf " ") eta

let ws =
  skip_while (function
    | '\x20' | '\x0a' | '\x0d' | '\x09' -> true
    | _ -> false)
;;

let failf fmt = Format.kasprintf fail fmt

let trace_pos msg =
  let* n = pos in
  let _ = msg, n in
  log "`%s` on pos %d" msg n;
  return ()
;;

let trace_avai msg =
  available
  >>= fun n ->
  let _ = msg, n in
  (* log "`%s` there are %d available." msg n; *)
  return ()
;;

let lchar c = ws *> char c
let parens p = char '(' *> trace_pos "after(" *> p <* trace_pos "before ')'" <* lchar ')'
let const = char '0' >>= fun c -> return (Printf.sprintf "%c" c)

type dispatch =
  { prio : dispatch -> expr t
  ; expr_basic : dispatch -> expr t
  ; expr_long : dispatch -> expr t
  ; expr : dispatch -> expr t
  }

let is_digit = function
  | '0' .. '9' -> true
  | _ -> false
;;

let to_digit c = Char.code c - Char.code '0'

let digit =
  any_char
  >>= function
  | '0' .. '9' as c -> return (Char.code c - Char.code '0')
  | _ -> fail ""
;;

let number =
  trace_pos "number" *> digit
  >>= fun h ->
  scan_state h (fun st c -> if is_digit c then Some ((10 * st) + to_digit c) else None)
;;

let is_alpha = function
  | 'a' .. 'z' | 'A' .. 'Z' -> true
  | _ -> false
;;

let alpha_c =
  any_char
  >>= function
  | c -> if is_alpha c then return c else fail (Format.sprintf "'%c' not a letter" c)
;;

let alpha_digit_c =
  satisfy (fun c -> is_alpha c || is_digit c || c = '_') <?> "not a alpha or digit"
;;

let is_keyword = function
  | "fun" | "in" | "let" | "rec" | "if" | "then" | "else" -> true
  | _ -> false
;;

let ident =
  let* i =
    let* h = ws *> alpha_c in
    let* tl = many alpha_digit_c in
    return (Base.String.of_char_list (h :: tl))
  in
  if is_keyword i
  then fail "got a keyword"
  else (* let () = log "got ident %S" i in *)
    return i
;;

let string s = trace_pos (Format.sprintf "string `%s`" s) *> string s

let pattern =
  fix (fun pattern ->
    fail ""
    <|> parens
          (return (fun a b ps -> PTuple (a, b, ps))
          <*> pattern
          <*> string "," *> ws *> pattern
          <*> many (string "," *> ws *> pattern))
    <|> (ws *> ident >>= fun v -> return (pvar v) <* trace_pos v))
;;

(* >>| fun x ->
  log "pattern %a parsed" Pprint.pp_pattern x;
  x *)

let keyword kwd =
  ws
  *> string kwd
  *> let* c = peek_char_fail in
     if is_alpha c || is_digit c
     then
       let* p = pos in
       failf "input is not a keyword '%s', pos = %d" kwd p
     else ws *> return (log "keyword '%s' parsed" kwd)
;;

let prio expr table =
  let len = Array.length table in
  let rec helper level =
    if level >= len
    then expr
    else (
      let xs = table.(level) in
      return (fun h tl ->
        log "helper returned h ='%a'" Parsetree.pp_expr h;
        log "                tl size = %d" (List.length tl);
        List.fold_left (fun acc (op, r) -> op acc r) h tl)
      <*> helper (level + 1)
      <*> many
            (choice
               (List.map
                  (fun (op, f) -> op *> helper (level + 1) >>= fun r -> return (f, r))
                  xs)))
  in
  helper 0
;;

let letdef erhs =
  return (fun isrec name ps rhs -> isrec, name, List.fold_right elam ps rhs)
  <*> (trace_pos "let"
       *> keyword "let"
       *> option NonRecursive (keyword "rec" >>| fun _ -> Recursive)
      <* ws)
  <*> pattern
  <*> many (ws *> pattern)
  <*> ws *> string "=" *> ws *> erhs
;;

(* The equivalent of [letdef] *)
let letdef0 erhs =
  let+ isrec =
    keyword "let" *> option NonRecursive (keyword "rec" >>| fun _ -> Recursive) <* ws
  in
  let+ name = pattern in
  (* TODO(Kakadu): not any pattern *)
  let+ ps = many pattern in
  let+ rhs = ws *> keyword "=" *> ws *> erhs in
  isrec, name, List.fold_right elam ps rhs
;;

let pack : dispatch =
  let open Format in
  let prio d =
    prio
      (d.expr_long d)
      [| [ ws *> string "=", eeq; ws *> string "<", elt; ws *> string ">", egt ]
       ; [ ws *> string "+", eadd; ws *> string "-", esub ]
       ; [ ws *> string "*", emul ]
      |]
  in
  let expr_basic d =
    trace_pos "expr_basic"
    *> fix (fun _self ->
         ws
         *> (fail ""
            <|> ws *> (number >>| fun n -> econst (const_int n))
            <|> parens
                  (return (fun a b xs -> etuple a b xs)
                  <*> (d.expr d <* ws)
                  <*> (string "," *> d.expr d <* ws)
                  <*> many (string "," *> d.expr d <* ws))
            <|> (ws *> ident >>| evar)
            <|> (keyword "fun" *> pattern
                >>= fun p ->
                (* let () = log "Got a abstraction over %a" Pprint.pp_pattern p in *)
                ws *> string "->" *> ws *> d.prio d >>= fun b -> return (elam p b))
            <|> (keyword "if" *> d.prio d
                >>= fun cond ->
                keyword "then" *> d.prio d
                >>= fun th ->
                keyword "else" *> d.prio d >>= fun el -> return (eite cond th el))
            <|> (letdef (d.prio d)
                >>= fun (isrec, ident, rhs) ->
                keyword "in" *> d.prio d >>= fun in_ -> return (elet ~isrec ident rhs in_)
                )))
  in
  let expr_long d =
    fix (fun _self ->
      many (ws *> (d.expr_basic d <|> parens (d.prio d)) <* ws)
      >>= function
      | [] -> fail "can't parse many expressions"
      | [ h ] -> return h
      | foo :: args -> return @@ eapp foo args)
  in
  { expr_basic; expr_long; prio; expr = prio }
;;

let value_binding = letdef (pack.expr pack) <* ws
let parse_pack p str = parse_string ~consume:All (p pack) str

type error = [ `Parse_error of string ]

let pp_error ppf = function
  | `Parse_error s -> Format.pp_print_string ppf s
;;

let parse str =
  Caml.Format.printf "parsing a string '%s'\n%!" str;
  Result.map_error (fun x -> `Parse_error x) (parse_pack pack.prio str)
;;

let wrap_parse_exn parser printer str =
  match parse_string ~consume:All parser str with
  | Result.Error e ->
    Format.eprintf "Error: %s\n" e;
    failwith "Error during parsing"
  | Ok r -> Format.printf "@[%a@]\n%!" printer r
;;

let parse_expr_exn str =
  (* Caml.Format.printf "parsing a string '%s'\n%!" str; *)
  match parse_pack pack.prio str with
  | Result.Error e ->
    Format.eprintf "Error: %s\n" e;
    failwith "Error during parsing"
  | Ok r -> Format.printf "@[%a@]\n%!" Pprint.pp_expr r
;;

let parse_vb_exn str =
  (* Caml.Format.printf "parsing a string '%s'\n%!" str; *)
  match parse_string ~consume:All value_binding str with
  | Result.Error e ->
    Format.eprintf "Error: %s\n" e;
    failwith "Error during parsing"
  | Ok r -> r
;;

let print_end_parse_exn input =
  Format.printf "@[%a@]\n%!" Pprint.pp_value_binding (parse_vb_exn input)
;;

let structure = many value_binding

let parse_structure str =
  parse_string ~consume:All structure str
  |> Result.map_error (fun s -> (`Parse_error s :> [> error ]))
;;

let test_stru str =
  match parse_string ~consume:All (many value_binding) str with
  | Result.Error e -> Format.eprintf "Error: %s\n" e
  | Ok xs -> List.iter (Format.printf "%a\n%!" Pprint.pp_value_binding) xs
;;

let%expect_test _ =
  test_stru
    {|
    let mul5 x = repeat 5 (fun acc -> x) 0
    let rec fac n = if n = 1 then n else n * (fac (n - 1))
    let main x = 32
     |};
  [%expect
    {|
    let mul5 x = repeat 5 (fun acc -> x) 0
    let rec fac n = if n = 1 then n else n * (fac (n - 1))
    let main x = 32 |}]
;;

let%expect_test _ =
  print_end_parse_exn {|let mul5 x = repeat 5 (fun acc -> x) 0
     |};
  [%expect {|
    let mul5 x = repeat 5 (fun acc -> x) 0 |}]
;;

let%expect_test _ =
  wrap_parse_exn (pack.expr pack) Pprint.pp_expr {|5+1|};
  [%expect {| (5 + 1) |}]
;;

let%expect_test _ =
  print_end_parse_exn {|let mul5 = 5+1 |};
  [%expect {| let mul5 = 5 + 1 |}]
;;

let%expect_test _ =
  print_end_parse_exn
    {|
    let foo = if n>0 then 1 else (if n>0 then 1 else (if n>0 then 1 else (if n>0 then 1 else 2)))
     |};
  [%expect
    {|
    let foo = if n > 0 then 1 else if n > 0 then 1 else if n > 0 then 1 else
                                                                        if n > 0
                                                                        then 1
                                                                        else 2 |}]
;;

let%expect_test _ =
  print_end_parse_exn
    {|
    let foo = let x = 5 in  let x = 5 in  let x = 5 in  let x = 5 in  let x = 5 in  let x = 5 in  let x = 5 in  let x = 5 in 11
     |};
  [%expect
    {|
    let foo = let x = 5 in let x = 5 in let x = 5 in let x = 5 in let x = 5 in
                                                                  let x = 5 in
                                                                  let x = 5 in
                                                                  let x = 5 in
                                                                  11 |}]
;;

let%expect_test _ =
  print_end_parse_exn {|
    let a = reeee (fun acc -> x)
     |};
  [%expect {| let a = reeee (fun acc -> x) |}]
;;

let%expect_test _ =
  print_end_parse_exn {| let fresh_1 x acc = x |};
  print_end_parse_exn {| let mul5 x = repeat 5 (fresh_1 x) 0 |};
  [%expect {|
    let fresh_1 x acc = x
    let mul5 x = repeat 5 (fresh_1 x) 0 |}]
;;

let%expect_test _ =
  parse_expr_exn {| 1+1 |};
  [%expect {|
    (1 + 1) |}]
;;

open Format

let%expect_test _ =
  let vb1 = parse_vb_exn {| let fresh_1 x acc = x |} in
  let vb2 = parse_vb_exn {| let mul5 x = repeat 5 (fresh_1 x) 0 |} in
  printf
    "@[%a@]\n\t~~[%d value bindings]~~>\n@[<v>%a@]\n"
    Pprint.pp_value_binding
    vb1
    2
    (pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf "@.") Pprint.pp_value_binding)
    [ vb1; vb2 ];
  [%expect
    {|
    let fresh_1 x acc = x
    	~~[2 value bindings]~~>
    let fresh_1 x acc = x
    let mul5 x = repeat 5 (fresh_1 x) 0
|}]
;;
