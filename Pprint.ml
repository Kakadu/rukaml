open Format
open Parsetree

let rec pp_letrec_helper ~sk ppf rhs =
  match rhs with
  | ELam (PVar x, rhs) ->
    fprintf ppf "%s " x;
    pp_letrec_helper ~sk ppf rhs
  | _ -> Format.fprintf ppf "= %a" sk rhs
;;

let group_applications l r =
  let rec loop acc = function
    | EApp (l, r) -> loop (r :: acc) l
    | other -> other, acc
  in
  loop [ r ] l
;;

let rec pp_expr_helper ?(ps = true) ppf = function
  | EConst n -> fprintf ppf "%d" n
  | EIf (c, th, el) ->
    (if ps then fprintf ppf "(%a)" else fprintf ppf "%a")
      (fun ppf ->
        fprintf
          ppf
          "@[<hov>@[if %a@ @]@[then %a@ @]@[else %a@]@]"
          no_pars
          c
          no_pars
          th
          no_pars)
      el
  | EVar s -> pp_print_string ppf s
  | EApp (EApp (EVar ("<" as op), l), r)
  | EApp (EApp (EVar ("<=" as op), l), r)
  | EApp (EApp (EVar (">" as op), l), r)
  | EApp (EApp (EVar (">=" as op), l), r)
  | EApp (EApp (EVar ("*" as op), l), r)
  | EApp (EApp (EVar ("/" as op), l), r)
  | EApp (EApp (EVar ("+" as op), l), r)
  | EApp (EApp (EVar ("=" as op), l), r) ->
    if ps
    then fprintf ppf "(%a %s %a)" maybe_pars l op maybe_pars r
    else fprintf ppf "%a %s %a" maybe_pars l op maybe_pars r
  | EApp (l, r) ->
    let grouped = group_applications l r in
    let lam_itself ppf (fexpr, args) =
      fprintf ppf "@[<hv>%a" maybe_pars fexpr;
      List.iteri (fun _ -> fprintf ppf " @[%a@]" maybe_pars) args;
      fprintf ppf "@]"
    in
    if ps then fprintf ppf "(%a)" lam_itself grouped else lam_itself ppf grouped
  | ELam (PVar name, e) -> fprintf ppf "@[(fun %s ->@[ %a@])" name no_pars e
  | ELet (flg, PVar name, body, in_) ->
    let rec_ =
      match flg with
      | Recursive -> "rec "
      | _ -> ""
    in
    fprintf ppf "@[<v>@[<hv>@[let %s%s " rec_ name;
    let args, body = group_lams body in
    List.iter (fprintf ppf "%s ") args;
    fprintf ppf "= @]";
    Format.fprintf ppf "@[<2>%a @]@[in @]@]" no_pars body;
    fprintf ppf "@[%a@]" no_pars in_;
    fprintf ppf "@]"

and no_pars ppf = pp_expr_helper ~ps:false ppf
and maybe_pars ppf = pp_expr_helper ~ps:true ppf

let pp_expr = pp_expr_helper ~ps:true

let pp_pattern ppf = function
  | Parsetree.PVar s -> fprintf ppf "%s" s
;;

let pp_value_binding ppf (is_rec, pat, rhs) =
  let () =
    (match is_rec with
     | Parsetree.Recursive -> fprintf ppf "@[<v2>@[let rec %a "
     | NonRecursive -> fprintf ppf "@[<v2>@[let %a ")
      pp_pattern
      pat
  in
  match group_lams rhs with
  | args, rhs ->
    List.iter (fprintf ppf "%s ") args;
    fprintf ppf "=@ @]@[%a@]@]" no_pars rhs
;;

open Typedtree

let rec pp_typ ppf { typ_desc } =
  match typ_desc with
  | V n -> fprintf ppf "'_%d" n
  | Prim s -> pp_print_string ppf s
  | Arrow (l, r) -> fprintf ppf "(%a -> %a)" pp_typ l pp_typ r
  | TLink t -> pp_typ ppf t
;;

let pp_scheme ppf = function
  | S (xs, t) -> fprintf ppf "forall %a . %a" Var_set.pp xs pp_typ t
;;

let pp_stru ppf vbs =
  open_vbox 0;
  List.iter (fprintf ppf "@[%a@]" pp_value_binding) vbs;
  close_box ()
;;

let structure = pp_stru
