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
  | "fun" | "in" | "let" | "rec" | "if" | "then" | "else" | "match" | "with" | "type" -> true
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

let pattern =
  fix (fun pattern ->
    fail ""
    <|> parens
          (return (fun a b ps -> PTuple (a, b, ps))
           <*> pattern
           <*> string "," *> ws *> pattern
           <*> many (string "," *> ws *> pattern))
    <|> (ws *> ident >>= fun v -> return (pvar v) <* trace_pos v)
    <|> (ws *> keyword "_" *> return PAny))
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
      [| [ ws *> string "=", eeq
         ; ws *> string "<=", ele
         ; ws *> string "<", elt
         ; ws *> string ">", egt
         ]
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
          <|> (ws *> char '(' *> char ')' >>| fun _ -> eunit)
          <|> (ws *> ident
               >>= function
               | "true" -> return @@ econst (const_bool true)
               | "false" -> return @@ econst (const_bool true)
               | _ -> fail "Not a boolean constant")
          <|> parens
                (return (fun a b xs -> etuple a b xs)
                 <*> (d.expr d <* ws)
                 <*> (string "," *> d.expr d <* ws)
                 <*> many (string "," *> d.expr d <* ws))
          <|> (ws *> ident >>| evar)
          <|> (
                  keyword "match" *> d.prio d >>= fun e -> ws
                  *> keyword "with" *>
                  (many (ws *> char '|' *> ws *> pattern >>= fun p -> ws *> string "->" *> ws *> d.prio d >>= fun e -> return (p, e)))
                  >>= function
                  | pe :: pes -> return (ematch e pe pes)
                  | _ -> fail "Pattern matching cases expected"
              )
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

let parse_pack p str = parse_string ~consume:All (p pack) str

type error = [ `Parse_error of string ]

let pp_error ppf = function
  | `Parse_error s -> Format.pp_print_string ppf s
;;

let parse str =
  Stdlib.Format.printf "parsing a string '%s'\n%!" str;
  Result.map_error (fun x -> `Parse_error x) (parse_pack pack.prio str)
;;

let parse_name_fabric regexp error_message =
  ws *> (take_while1 (fun ch -> is_alpha ch || ch = '\'')) >>= fun chs ->
  if Str.string_match (Str.regexp regexp) chs 0 then return chs
  else fail error_message

let parse_type_name =
  let regexp = "^[a-z_][a-zA-Z0-9_]*$" in
  let message = "type name expected" in
  parse_name_fabric regexp message

let parse_type_param_name =
  let regexp = "^'[a-zA-Z][a-zA-Z0-9_]*$" in
  let message = "type param name expected" in
  parse_name_fabric regexp message

let parse_constructor_name =
  let regexp = "^[A-Z][a-zA-Z0-9_]*$" in
  let message = "not a constructor name" in
  parse_name_fabric regexp message

let parse_ct_var =
  ws *> parse_type_name <|> ws *> parse_type_param_name >>| fun name -> CTVar name

let parse_one_type_param = ws *> parse_type_param_name >>| fun param -> [ param ]

let parse_type_param_tuple =
  ws *> (char '(')
  *> ws *> (sep_by (ws *> (char ',')) (ws *> parse_type_param_name))
  >>= function
  | frst :: scnd :: rest -> return (frst :: scnd :: rest) <* ws *> (char ')')
  | _ -> fail "tuple of param names expected"

let parse_type_params =
  ws *> parse_one_type_param
  <|> parens (ws *> parse_one_type_param)
  <|> ws *> parse_type_param_tuple
  <|> return []

let parse_ctconstr parse_core_type =
  ws *> parse_core_type >>= fun arg ->
  ws *> parse_type_name >>| fun consructor -> CTConstr (arg, consructor)

let parse_ctarrow parse_core_type =
  fix (fun opp ->
      ws *> parse_core_type >>= fun operand ->
      ws *> (string "->") *> ws *> opp
      >>| (fun operand2 -> CTArrow (operand, operand2))
      <|> return operand)

let parse_cttuple parse_core_type =
  parse_core_type >>= fun first ->
  many (ws *> char '*' *> ws *> parse_core_type) >>= function
  | [] -> return first  (* Not a tuple, just return the single type *)
  | second :: rest -> return (CTTuple (first, second, rest))

let parse_core_type =
  fix (fun ct ->
      let ct = ws *> parse_ct_var <|> parens ct in
      let ct = ws *> (parse_ctconstr ct) <|> ct in
      let ct = ws *> (parse_cttuple ct) <|> ct in
      let ct = ws *> (parse_ctarrow ct) <|> ct in
      ct)

let parse_variants =
  let parse_variant =
    ws *> (char '|') *> ws *> parse_constructor_name >>= fun name ->
    ws *> (string "of") *> ws *> parse_core_type
    >>| (fun core_type -> (name, Some core_type))
    <|> return (name, None)
  in
  many parse_variant >>= function
  | var :: vars -> return (TKVariants (var, vars))
  | _ -> fail "is not variants"

let parse_record = fail "not implemented" (* TODO *)
let parse_allias = ws *> parse_core_type >>| fun core_type -> TKAlias core_type

let parse_type_kinds =
  ws *> parse_allias <|> ws *> parse_variants <|> ws *> parse_record

let type_definition params name kind =
  { typedef_params = params; typedef_name = name; typedef_kind = kind }

let parse_type_definition =
  ws *> parse_type_params >>= fun params ->
  ws *> parse_type_name >>= fun name ->
  ws *> (char '=') *> ws *> parse_type_kinds >>| fun kind ->
  type_definition params name kind

let parse_stype =
  ws *> (string "type") *> parse_type_definition >>= fun frst ->
  sep_by (ws *> (string "and")) parse_type_definition >>| fun rest ->
  SType (frst, rest)

let value_binding = letdef (pack.expr pack) <* ws

let structure = many1
(
  (value_binding  >>| fun vb -> SLet vb)
  <|>
  parse_stype
)

let parse_structure str =
  parse_string ~consume:All structure str
  |> Result.map_error (fun s -> (`Parse_error s :> [> error ]))
;;

(** {1} Testing stuff *)

let parse_pat_exn str =
  match parse_string ~consume:All pattern str with
  | Result.Error e ->
    Format.eprintf "Error: %s\n" e;
    failwith "Error during parsing of pattern"
  | Ok r -> r
;;

let parse_vb_exn str =
  (* Stdlib.Format.printf "parsing a string '%s'\n%!" str; *)
  match parse_string ~consume:All value_binding str with
  | Result.Error e ->
    Format.eprintf "Error: %s\n" e;
    failwith "Error during parsing"
  | Ok r -> r
;;
