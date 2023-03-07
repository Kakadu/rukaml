open Parsetree
open Typedtree
open Format

let pp_typ_hum =
  let open Format in
  let rec pp_typ ppf t =
    match t.typ_desc with
    | Prim s -> fprintf ppf "%s" s
    | V n -> fprintf ppf "'_%d" n
    | Arrow (l, r) -> fprintf ppf "(%a -> %a)" pp_typ l pp_typ r
    | TLink ty -> pp_typ ppf ty
    | TProd (a, b, ts) ->
      fprintf ppf "@[(%a, %a" pp_typ a pp_typ b;
      List.iter (fprintf ppf ", %a" pp_typ) ts;
      fprintf ppf ")@]"
  in
  pp_typ
;;

let pp_hum =
  let open Format in
  let rec expr ppf = function
    | TUnit -> fprintf ppf "unit"
    | TConst c -> Pprint.pp_const ppf c
    | TVar (name, _) -> fprintf ppf "%s" name
    | TIf (cond, th, el, _) ->
      fprintf ppf "if %a then %a else %a" expr cond expr th expr el
    | TLam (pat, e, _) -> fprintf ppf "fun %a -> %a" pp_pat pat expr e
    | TApp (TApp (TVar ("+", _), l, _), r, _) -> fprintf ppf "(%a + %a)" expr l expr r
    | TApp (TApp (TVar ("*", _), l, _), r, _) -> fprintf ppf "(%a * %a)" expr l expr r
    | TApp (TApp (TVar ("-", _), l, _), r, _) -> fprintf ppf "(%a - %a)" expr l expr r
    | TApp (TApp (TVar ("=", _), l, _), r, _) -> fprintf ppf "(%a = %a)" expr l expr r
    | TApp (l, r, _) -> fprintf ppf "(%a %a)" expr l expr r
    | TLet (Parsetree.Recursive, pat, S (_vars, ty), rhs, wher) ->
      fprintf ppf "let rec %a : %a = %a in %a" pp_pat pat pp_typ ty expr rhs expr wher
    | TLet (NonRecursive, pat, S (_vars, ty), rhs, wher) ->
      fprintf ppf "@[let %a : %a = %a in@]@,%a" pp_pat pat pp_typ ty expr rhs expr wher
    | TTuple (a, b, es, _) ->
      fprintf ppf "@[(%a, %a" expr a expr b;
      List.iter (fprintf ppf ", %a" expr) es;
      fprintf ppf ")@]"
  and pp_typ = pp_typ_hum
  and pp_pat ppf s = fprintf ppf "%s" s in
  fun ppf e -> fprintf ppf "@[<v>%a@]" expr e
;;

let rec pp_pattern ppf = function
  | Parsetree.PVar s -> Format.fprintf ppf "%s" s
  | PTuple (h1, h2, rest) ->
    fprintf
      ppf
      "(%a, %a, %a)"
      pp_pattern
      h1
      pp_pattern
      h2
      (pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf " ") pp_pattern)
      rest
;;

let pp_vb_hum ppf { tvb_flag; tvb_pat; tvb_body; tvb_typ } =
  fprintf
    ppf
    "@[let %s%a: @[%a@] =%a@]"
    (match tvb_flag with
     | Recursive -> "rec "
     | NonRecursive -> "")
    pp_pattern
    tvb_pat
    pp_typ_hum
    (match tvb_typ with
     | S (_, typ) -> typ)
    pp_hum
    tvb_body
;;

let pp_stru ppf stru =
  open_vbox 0;
  pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf "@,") pp_vb_hum ppf stru;
  close_box ()
;;
