open AST
open Format

let pp_ident = pp_print_string

let rec pp_expr ppf = function
  | EConst n -> fprintf ppf "%d" n
  | EVar v -> pp_ident ppf v
  | EBinop (op, l, r) -> fprintf ppf "(%a%s%a)" pp_expr l op pp_expr r

let semic_list eta = pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf ";@ ") eta

let pp_stmt =
  let rec helper ppf = function
    | Assgn (v, e) -> fprintf ppf "%s:=%a;" v pp_expr e
    | Ite (cond, th, el) ->
        fprintf ppf "if %a then %a else %a fi;" pp_expr cond (semic_list helper)
          th (semic_list helper) el
    | While (cond, body) ->
        fprintf ppf "while %a do %a done;" pp_expr cond (semic_list helper) body
  in
  helper

let show_ident eta = asprintf "%a" pp_ident eta
let show_expr eta = asprintf "%a" pp_expr eta
let show_stmt eta = asprintf "%a" pp_stmt eta
