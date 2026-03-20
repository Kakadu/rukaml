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
let ws = skip_while Base.Char.is_whitespace
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

let parens p =
  char '(' *> trace_pos "after '('" *> p <* trace_pos "before ')'" <* lchar ')'
;;

let apostrophes p =
  char '\'' *> trace_pos "after \'" *> p <* trace_pos "before \'" <* lchar '\''
;;

let quotes p =
  char '"' *> trace_pos "after '\"'" *> p <* trace_pos "before '\"'" <* lchar '"'
;;

let any_char_except chs =
  any_char
  >>= function
  | x when List.mem x chs -> fail "any_char_except"
  | x -> return x
;;

let brackets p =
  char '[' *> char '|' *> trace_pos "after '[|'" *> p
  <* trace_pos "before '|]'"
  <* lchar '|'
  <* char ']'
;;

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

let parse_name regexp error_message =
  let* chs = ws *> take_while1 is_char_valid_for_name in
  if is_keyword chs
  then fail "unexpected keyword"
  else if Str.string_match (Str.regexp regexp) chs 0
  then return chs
  else fail error_message
;;

let var_name =
  let regexp = "^[a-z_][a-zA-Z0-9_']*$" in
  let message = "not a variable name" in
  parse_name regexp message
;;

let type_name =
  let regexp = "^[a-z_][a-zA-Z0-9_']*$" in
  let message = "not a type name" in
  parse_name regexp message
;;

let type_param_name =
  let regexp = "^'[a-zA-Z][a-zA-Z0-9_]*$" in
  let message = "not a type param name" in
  parse_name regexp message
;;

let constructor_name =
  let regexp = "^[A-Z][a-zA-Z0-9_']*$" in
  let message = "not a constructor name" in
  parse_name regexp message
;;

let ident = var_name

type dispatch_patt =
  { patt_basic : dispatch_patt -> pattern t
  ; patt_cons : dispatch_patt -> pattern t
  ; patt_tuple : dispatch_patt -> pattern t
  ; patt : dispatch_patt -> pattern t
  }

let escape_seq =
  char '\\'
  *> choice
       [ char 'n' *> return '\n'
       ; char 'r' *> return '\r'
       ; char 't' *> return '\t'
       ; char 'b' *> return '\b'
       ; char '\\' *> return '\\'
       ; char '\'' *> return '\''
       ; char '\"' *> return '\"'
       ]
;;

let constant =
  ws *> fail ""
  <|> (take_while1 is_digit >>| fun chs -> const_int (int_of_string chs))
  <|> (apostrophes escape_seq >>| const_char)
  <|> (apostrophes (any_char_except [ '\''; '\\' ]) >>| fun ch -> const_char ch)
  <|> quotes
        (many (any_char_except [ '"'; '\\' ] <|> escape_seq)
         >>| fun chs -> const_string (Base.String.of_char_list chs))
  <|> (var_name
       >>= function
       | "true" -> return (const_bool true)
       | "false" -> return (const_bool false)
       | _ -> fail "Not a boolean constant")
;;

let constructor ~atom ~item mk_constructor =
  let constant_constr =
    let* name = ws *> constructor_name in
    return (mk_constructor name [])
  in
  let* name = ws *> constructor_name in
  (let* args = parens (sep_by (ws <* char ',') item) in
   return (mk_constructor name args))
  <|> (atom <|> constant_constr >>| fun arg -> mk_constructor name [ arg ])
  <|> return (mk_constructor name [])
;;

let patt_basic d =
  ws
  *> fix (fun _self ->
    parens (d.patt d)
    <|> char '(' *> char ')' *> return PUnit
    <|> (constant >>| fun x -> PConst x)
    <|> (var_name >>= fun v -> return (pvar v) <* trace_pos v)
    <|> char '[' *> ws *> char ']' *> return pnil
    <|> (char '['
         *> ws
         *>
         let* first = d.patt d in
         (let* rest = many (ws *> char ';' *> d.patt d) in
          return (pcons first (List.fold_right pcons rest pnil)))
         <* ws
         <* char ']')
    <|> constructor
          ~atom:(d.patt_basic d)
          ~item:(d.patt_basic d <|> d.patt_cons d <|> parens (d.patt_tuple d))
          pconstruct
    <|> char '_' *> return PAny)
;;

let patt_cons d =
  ws
  *> fix (fun _self ->
    return (fun head tail -> pcons head tail)
    <*> d.patt_basic d
    <*> ws *> string "::" *> ws *> d.patt_cons d
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
  <*> ws *> pattern
  <*> many (ws *> pattern)
  <*> ws *> string "=" *> ws *> erhs
;;

(* The equivalent of [letdef] *)
let letdef0 erhs =
  let+ isrec =
    keyword "let" *> option NonRecursive (keyword "rec" *> return Recursive) <* ws
  in
  let+ name = pattern in
  (* TODO(Kakadu): not any pattern *)
  let+ ps = many pattern in
  let+ rhs = ws *> keyword "=" *> ws *> erhs in
  isrec, name, List.fold_right elam ps rhs
;;

type dispatch =
  { prio : dispatch -> expr t
  ; expr_basic : dispatch -> expr t
  ; expr_long : dispatch -> expr t
  ; expr_tuple : dispatch -> expr t
  ; expr : dispatch -> expr t
  }

let pack : dispatch =
  let open Format in
  let expr_tuple d =
    let* () = ws *> trace_pos "expr_tuple" in
    let* x1 = d.prio d in
    return (fun x2 xs -> etuple x1 x2 xs)
    <*> ws *> char ',' *> d.prio d
    <*> many (ws *> char ',' *> d.prio d)
    <|> return x1
  in
  let prio d =
    let* () = ws *> trace_pos "prio" in
    fix (fun _self ->
      prio
        (d.expr_long d)
        [| [ ws *> string "||", elor ]
         ; [ ws *> string "&&", eland ]
         ; [ ws *> string "=", eeq
           ; ws *> string "<>", ene
           ; ws *> string "<=", ele
           ; ws *> string ">=", ege
           ; ws *> string "<", elt
           ; ws *> string ">", egt
           ]
         ; [ ws *> string "::", econs ]
         ; [ ws *> string "+", eadd; ws *> string "-", esub ]
         ; [ ws *> string "*", emul ]
        |])
  in
  let expr_basic d =
    let* () = ws *> trace_pos "expr_basic" in
    fix (fun _self ->
      fail ""
      <|> (constant >>| fun x -> EConst x)
      <|> char '(' *> char ')' *> return EUnit
      <|> char '[' *> ws *> char ']' *> return enil
      <|> (constructor_name >>| fun name -> EConstruct (name, []))
      <|> (char '['
           *> ws
           *>
           let* first = d.expr_tuple d in
           (let* rest = many (ws *> char ';' *> d.expr_tuple d) in
            return (econs first (List.fold_right econs rest enil)))
           <|> return (econs first enil)
           <* ws
           <* char ']')
      <|> brackets
            (return (fun h tl -> earray (h :: tl))
             <*> (d.expr_tuple d <* ws)
             <*> many (string ";" *> d.expr_tuple d <* ws)
             <|> return @@ earray [])
      <|> (var_name
           >>= fun v ->
           char '.' *> char '(' *> number
           <* char ')'
           >>= (fun i ->
           string " <- " *> d.expr d
           >>= (fun e -> return @@ eapp (evar "set") [ evar v; econst (const_int i); e ])
           <|> return @@ eapp (evar "get") [ evar v; econst (const_int i) ])
           <|> return (evar v))
      <|> (let parse_case =
             let* () = ws <* char '|' in
             let* p = pattern in
             let* () = ws <* string "->" in
             let* e = d.expr_tuple d in
             return (p, e)
           in
           let first =
             parse_case
             <|>
             let* p = pattern in
             let* () = ws *> string "->" *> ws in
             let* e = d.expr_tuple d in
             return (p, e)
           in
           let* subject =
             keyword "match" *> ws *> d.expr_tuple d <* ws <* keyword "with"
           in
           let* case = first in
           let* cases = many parse_case in
           return (ematch subject case cases))
      <|> (keyword "fun" *> many1 pattern
           >>= fun ps ->
           ws *> string "->" *> ws *> d.expr d
           >>= fun b -> return (List.fold_right (fun patt acc -> elam patt acc) ps b))
      <|> (keyword "if" *> d.expr_tuple d
           >>= fun cond ->
           keyword "then" *> d.expr_tuple d
           >>= fun th ->
           keyword "else" *> d.expr_tuple d >>= fun el -> return (eite cond th el))
      <|> (letdef (d.expr_tuple d)
           >>= fun (isrec, ident, rhs) ->
           ws *> keyword "in" *> d.expr d
           >>= fun in_ -> return (elet ~isrec ident rhs in_)))
  in
  let expr_long d =
    let* () = ws *> trace_pos "expr_long" in
    fix (fun _self ->
      many (ws *> (d.expr_basic d <|> parens (d.expr d)) <* ws)
      >>= function
      | [] -> fail "can't parse many expressions"
      | [ h ] -> return h
      (* TODO? > fix these kludges for adt constructors *)
      | [ EConstruct (name, []); ETuple (arg1, arg2, args) ] ->
        return (EConstruct (name, arg1 :: arg2 :: args))
      | [ EConstruct (name, []); arg ] -> return (EConstruct (name, [ arg ]))
      (* < *)
      | foo :: args -> return @@ eapp foo args)
  in
  { expr_basic; expr_long; prio; expr_tuple; expr = expr_tuple }
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

type dispatch_core_type =
  { core_type_arrow : dispatch_core_type -> core_type t
  ; core_type_tuple : dispatch_core_type -> core_type t
  ; core_type_atom : dispatch_core_type -> core_type t
  ; core_type_app : dispatch_core_type -> core_type t
  ; core_type : dispatch_core_type -> core_type t
  }

let core_type_atom d =
  fix (fun _self ->
    ws
    *> choice
         [ (type_param_name >>| fun name -> Ptyp_var name)
         ; (type_name >>| fun name -> Ptyp_constr (name, []))
         ; parens (d.core_type d)
         ])
;;

let core_type_app d =
  ws
  *> fix (fun _self ->
    (let* args =
       choice
         [ parens
             (let* arg1 = d.core_type d in
              let* args = many (ws *> char ',' *> d.core_type d) in
              return (arg1 :: args))
         ; (d.core_type_atom d >>| fun arg -> [ arg ])
         ]
     in
     let* first = ws *> type_name in
     let* rest = many (ws *> type_name) in
     return
       (Base.List.fold
          ~f:(fun arg f -> Ptyp_constr (f, [ arg ]))
          ~init:(Ptyp_constr (first, args))
          rest))
    <|> d.core_type_atom d)
;;

let core_type_tuple d =
  fix (fun _self ->
    let* first = ws *> d.core_type_app d in
    many (ws *> char '*' *> d.core_type_app d)
    >>= function
    | [] -> return first
    | second :: rest -> return (Ptyp_tuple (first, second, rest)))
;;

let core_type_arrow d =
  fix (fun _self ->
    let* operand = ws *> d.core_type_tuple d in
    ws *> string "->" *> ws *> d.core_type_arrow d
    >>| (fun operand2 -> Ptyp_arrow (operand, operand2))
    <|> return operand)
;;

let core_type d =
  choice
    [ d.core_type_arrow d; d.core_type_tuple d; d.core_type_app d; d.core_type_atom d ]
;;

(** parses <core_type> in [ type t = <core_type> ] *)
let core_type_alias =
  core_type { core_type; core_type_arrow; core_type_tuple; core_type_app; core_type_atom }
;;

(** parses <core_typeK> in [ type t = Foo of <core_type1> * ... * <core_typeN> ] *)
let core_type_constr_arg =
  let helper d =
    choice
      [ d.core_type_app d
      ; parens (d.core_type_arrow d)
      ; parens (d.core_type_tuple d)
      ; d.core_type_atom d
      ]
  in
  helper { core_type; core_type_arrow; core_type_tuple; core_type_app; core_type_atom }
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
  let variant =
    let* name = ws *> constructor_name in
    (let* () = ws *> keyword "of" in
     let* args = sep_by1 (ws *> char '*') core_type_constr_arg in
     return (name, args))
    <|> return (name, [])
  in
  let* pty_params = ws *> type_params in
  let* pty_name = ws *> type_name in
  let* () = ws <* char '=' in
  let* v1 = variant <|> ws *> char '|' *> variant in
  let* vs = many (ws *> char '|' *> variant) in
  return { pty_name; pty_params; pty_kind = Ptype_variant (v1, vs); pty_manifest = None }
;;

let type_kind_alias =
  let* pty_params = ws *> type_params in
  let* pty_name = ws *> type_name in
  let* () = ws <* char '=' in
  let* pty_manifest = core_type_alias >>| Option.some in
  return { pty_name; pty_params; pty_kind = Ptype_abstract; pty_manifest }
;;

let type_kind_abstract =
  let* pty_params = ws *> type_params in
  let* pty_name = ws *> type_name in
  return { pty_name; pty_params; pty_kind = Ptype_abstract; pty_manifest = None }
;;

let single_type_declaration =
  choice [ type_kind_variants; type_kind_alias; type_kind_abstract ]
;;

let type_declaration =
  let* () = ws <* string "type" in
  let* frst = ws *> single_type_declaration in
  let* rest = many (ws *> string "and" *> single_type_declaration) in
  return (frst, rest)
;;

let value_binding = letdef (pack.expr pack) <* ws
let skip_separator = option () (ws *> string ";;" *> return ())

let structure =
  many1
    (value_binding
     >>| (fun vb -> Pstr_value vb)
     <|> (type_declaration >>| fun td -> Pstr_type td)
     <* skip_separator)
;;

let parse_structure str =
  parse_string ~consume:All (structure <* ws <* end_of_input) str
  |> Result.map_error (fun s -> (`Parse_error s :> [> error ]))
;;

(** TODO: make preprocessing more flexible (and maybe move it to separated module) *)

let preprocessing =
  let comment p = string "(*" *> ws *> p <* ws <* string "*)" in
  let rule_skip =
    let* _ = comment (string "[begin skip]") in
    let* _ = many_till any_char (comment (string "[end skip]")) in
    return ()
  in
  let rule_default = string "(*" *> many_till any_char (string "*)") *> return () in
  let any_rule = rule_skip <|> rule_default <|> return () in
  many_till (any_rule *> any_char) end_of_input >>| Base.String.of_list
;;

let make_preprocessing_exn str =
  match parse_string ~consume:All preprocessing str with
  | Error err -> failwith (Format.sprintf "preprocessing error: %s" err)
  | Ok s -> s
;;

(** {1} Testing stuff *)

let parse_pat_exn str =
  match parse_string ~consume:All (pattern <* ws <* end_of_input) str with
  | Result.Error e ->
    Format.eprintf "Error: %s\n" e;
    failwith "Error during parsing of pattern"
  | Ok r -> r
;;

let parse_vb_exn str =
  (* Stdlib.Format.printf "parsing a string '%s'\n%!" str; *)
  match parse_string ~consume:All (value_binding <* ws <* end_of_input) str with
  | Result.Error e ->
    Format.eprintf "Error: %s\n" e;
    failwith "Error during parsing"
  | Ok r -> r
;;

let value_bindings = many1 (value_binding <* skip_separator)

let parse_value_bindings str =
  parse_string ~consume:All (value_bindings <* ws <* end_of_input) str
  |> Result.map_error (fun s -> (`Parse_error s :> [> error ]))
;;

let core_type : core_type t = core_type_alias
