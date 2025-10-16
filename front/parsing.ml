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

let is_char_valid_for_name = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '\'' | '_' -> true
  | _ -> false
;;

let is_keyword = function
  | "fun"
  | "in"
  | "let"
  | "rec"
  | "if"
  | "then"
  | "else"
  | "match"
  | "with"
  | "type"
  | "and"
  | "of"
  | "_" -> true
  | _ -> false
;;

let string s = trace_pos (Format.sprintf "string `%s`" s) *> string s

(* >>| fun x ->
  log "pattern %a parsed" Pprint.pp_pattern x;
  x *)

let keyword kwd =
  ws
  *> string kwd
  *> let* c = peek_char_fail in
     if is_char_valid_for_name c
     then
       let* p = pos in
       failf "input is not a keyword '%s', pos = %d" kwd p
     else ws *> return (log "keyword '%s' parsed" kwd)
;;

let name_fabric regexp error_message =
  let* chs = ws *> take_while1 is_char_valid_for_name in
  if is_keyword chs
  then fail "unexpected keyword"
  else if Str.string_match (Str.regexp regexp) chs 0
  then return chs
  else fail error_message
;;

let var_name =
  let regexp = "^[a-z_][a-zA-Z0-9_]*$" in
  let message = "not a variable name" in
  name_fabric regexp message
;;

let constructor_name =
  let regexp = "^[A-Z][a-zA-Z0-9_]*$" in
  let message = "not a constructor name" in
  name_fabric regexp message
;;

let type_name =
  let regexp = "^[a-z_][a-zA-Z0-9_]*$" in
  let message = "not a type name" in
  name_fabric regexp message
;;

let type_param_name =
  let regexp = "^'[a-zA-Z][a-zA-Z0-9_]*$" in
  let message = "not a type param name" in
  name_fabric regexp message
;;

let constructor_name =
  let regexp = "^[A-Z][a-zA-Z0-9_]*$" in
  let message = "not a constructor name" in
  name_fabric regexp message
;;

let ident = var_name

type dispatch_patt =
  { patt_basic : dispatch_patt -> pattern t
  ; patt_cons : dispatch_patt -> pattern t
  ; patt_tuple : dispatch_patt -> pattern t
  ; patt : dispatch_patt -> pattern t
  }

let pnil = PConstruct ("Nil", None)
let enil = EConstruct ("Nil", None)
let pcons hd tl = PConstruct ("Cons", Some (PTuple (hd, tl, [])))
let econs hd tl = EConstruct ("Cons", Some (ETuple (hd, tl, [])))

let patt_basic d =
  ws
  *> fix (fun _self ->
    fail ""
    <|> parens (d.patt d)
    <|> char '_' *> return PAny
    <|> (var_name >>= fun v -> return (pvar v) <* trace_pos v)
    <|> string "[]" *> return pnil
    <|> (char '['
         *> ws
         *>
         let* first = d.patt d in
         (let* rest = many (ws *> char ';' *> d.patt d) in
          return (pcons first (List.fold_right pcons rest pnil)))
         <|> return (pcons first pnil)
         <* ws
         <* char ']')
    <|> let* name = ws *> constructor_name in
        (let* patt = ws *> d.patt_basic d in
         return (PConstruct (name, Some patt)))
        <|> return (PConstruct (name, None)))
;;

let patt_cons d =
  ws
  *> fix (fun _self ->
    fail ""
    <|> (return (fun head tail -> pcons head tail)
         <*> d.patt_basic d
         <*> ws *> string "::" *> ws *> d.patt_cons d)
    <|> d.patt_basic d)
;;

let patt_tuple d =
  ws
  *> fix (fun _self ->
    return (fun a b xs -> PTuple (a, b, xs))
    <*> (d.patt_cons d <* ws)
    <*> (char ',' *> d.patt_cons d <* ws)
    <*> many (char ',' *> d.patt_cons d <* ws))
;;

let pattern : pattern t =
  let patt = fun d -> d.patt_tuple d <|> d.patt_cons d <|> d.patt_basic d in
  patt { patt; patt_basic; patt_cons; patt_tuple }
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
          <|> ws *> char '(' *> char ')' *> return eunit
          <|> ws *> char '[' *> char ']' *> return enil
          <|> (ws *> var_name
               >>= function
               | "true" -> return @@ econst (const_bool true)
               | "false" -> return @@ econst (const_bool true)
               | _ -> fail "Not a boolean constant")
          <|> (char '['
               *> ws
               *>
               let* first = d.prio d in
               (let* rest = many (ws *> char ';' *> d.prio d) in
                return (econs first (List.fold_right econs rest enil)))
               <|> return (econs first enil)
               <* ws
               <* char ']')
          <|> parens
                (return (fun a b xs -> etuple a b xs)
                 <*> (d.expr d <* ws)
                 <*> (string "," *> d.expr d <* ws)
                 <*> many (string "," *> d.expr d <* ws))
          <|> (ws *> var_name >>| evar)
          <|> (let* name = ws *> constructor_name in
               (let* patt = ws *> d.expr_basic d in
                return (EConstruct (name, Some patt)))
               <|> return (EConstruct (name, None)))
          <|> (let parse_case =
                 ws *> char '|' *> ws *> pattern
                 >>= fun p ->
                 ws *> string "->" *> ws *> d.prio d >>= fun e -> return (p, e)
               in
               keyword "match" *> ws *> d.prio d
               >>= fun e ->
               ws *> keyword "with" *> many parse_case
               >>= function
               | pe :: pes -> return (ematch e pe pes)
               | _ -> fail "Pattern matching cases expected")
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

let type_param_tuple =
  ws *> char '(' *> ws *> sep_by (ws *> char ',') (ws *> type_param_name)
  >>= function
  | frst :: scnd :: rest -> return (frst :: scnd :: rest) <* ws *> char ')'
  | _ -> fail "tuple of param names expected"
;;

let core_type_var = ws *> (type_name <|> type_param_name >>| fun name -> CTVar name)

let core_type_arrow core_type =
  fix (fun self ->
    let* operand = ws *> core_type in
    ws *> string "->" *> ws *> self
    >>| (fun operand2 -> CTArrow (operand, operand2))
    <|> return operand)
;;

let core_type_tuple core_type =
  let* first = ws *> core_type in
  many (ws *> char '*' *> ws *> core_type)
  >>= function
  | [] -> return first
  | second :: rest -> return (CTTuple (first, second, rest))
;;

let core_type =
  ws
  *> fix (fun self ->
    let prims =
      fail ""
      <|> (type_name >>| fun v -> CTConstr (v, []))
      <|> (type_param_name >>| fun v -> CTConstr (v, []))
    in
    let prims =
      (let* arg = prims in
       let* name = ws *> type_name in
       return (CTConstr (name, [ arg ])))
      <|> prims
    in
    let self = parens self <|> prims in
    let self = core_type_tuple self <|> self in
    let self = core_type_arrow self <|> self in
    (let* ct = char '(' *> ws *> self in
     let* cts = many (ws *> char ',' *> self) in
     let* name = ws *> char ')' *> type_name in
     return (CTConstr (name, ct :: cts)))
    <|> self)
;;

let type_params =
  ws
  *> (type_param_name
      >>| (fun param -> [ param ])
      <|> parens (type_param_name >>| fun param -> [ param ])
      <|> type_param_tuple
      <|> return [])
;;

let type_kind_variants =
  let parse_variant =
    let* name = ws *> char '|' *> ws *> constructor_name in
    (let* ct = ws *> keyword "of" *> ws *> core_type in
     return (name, Some ct))
    <|> return (name, None)
  in
  many parse_variant
  >>= function
  | var :: vars -> return (KVariants (var, vars))
  | _ -> fail "is not variants"
;;

let type_kind_alias = ws *> core_type >>| fun core_type -> KAbstract (Some core_type)
let type_kind = ws *> (type_kind_variants <|> type_kind_alias)

let single_type_definition =
  let* params = ws *> type_params in
  let* name = ws *> type_name in
  let* kind = ws *> char '=' *> ws *> type_kind in
  return { typedef_params = params; typedef_name = name; typedef_kind = kind }
;;

let type_definition =
  ws
  *> string "type"
  *>
  let* frst = ws *> single_type_definition in
  let* rest = many (ws *> string "and" *> single_type_definition) in
  return (frst, rest)
;;

let value_binding = letdef (pack.expr pack) <* ws
let value_bindings = many1 value_binding

let structure =
  many1
    (value_binding >>| (fun vb -> SValue vb) <|> (type_definition >>| fun td -> SType td))
;;

let parse_structure str =
  parse_string ~consume:All structure str
  |> Result.map_error (fun s -> (`Parse_error s :> [> error ]))
;;

let parse_value_bindings str =
  parse_string ~consume:All (many1 value_binding) str
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
