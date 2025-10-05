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

let rec pp_pattern ppf = function
  | Parsetree.PVar s -> fprintf ppf "@[%s@]" s
  | PTuple (pa, pb, ps) ->
    fprintf ppf "@[(%a" pp_pattern pa;
    List.iter (fprintf ppf ", %a" pp_pattern) (pb :: ps);
    fprintf ppf ")@]"
  | PAny -> fprintf ppf "_"
  | PConstruct (name, None) -> fprintf ppf "%s" name
  | PConstruct (name, Some arg) ->
    fprintf ppf "%a (" pp_print_string name;
    pp_pattern ppf arg;
    fprintf ppf ")"
;;

let pp_const ppf = function
  | PConst_bool b -> fprintf ppf "%b" b
  | PConst_int n -> fprintf ppf "%d" n
;;

let pp_flg ppf = function
  | Recursive -> Format.fprintf ppf "rec "
  | NonRecursive -> ()
;;

let rec pp_expr_helper ?(ps = true) ppf = function
  | EUnit -> fprintf ppf "()"
  | EConst n -> pp_const ppf n
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
  | EApp (EApp (EVar ("-" as op), l), r)
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
  | ELam (pat, e) -> fprintf ppf "@[(fun %a ->@[ %a@])" pp_pattern pat no_pars e
  | ELet (Recursive, PTuple _, _, _) -> failwith "Should not be representable in types"
  | ELet (flg, pat, body, in_) ->
    let rec_ =
      match flg with
      | Recursive -> "rec "
      | _ -> ""
    in
    fprintf ppf "@[<v>@[<hv>@[let %s%a " rec_ pp_pattern pat;
    let args, body = group_lams body in
    List.iter (fprintf ppf "%a " pp_pattern) args;
    fprintf ppf "= @]";
    Format.fprintf ppf "@[<2>%a @]@[in @]@]" no_pars body;
    fprintf ppf "@[%a@]" no_pars in_;
    fprintf ppf "@]"
  | ETuple (h1, h2, hs) ->
    fprintf ppf "@[(%a, " no_pars h1;
    Format.fprintf
      ppf
      "%a"
      (pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf ", ") no_pars)
      (h2 :: hs);
    fprintf ppf ")@]"
  | EConstruct (name, None) -> fprintf ppf "%s" name
  | EConstruct (name, Some arg) ->
    fprintf ppf "%s (" name;
    pp_expr ppf arg;
    fprintf ppf ")"
  | EMatch (e, (pe, pes)) ->
    fprintf ppf "match ";
    pp_expr ppf e;
    fprintf ppf " with\n";
    let helper (p, e) =
      fprintf ppf "| ";
      pp_pattern ppf p;
      fprintf ppf " -> ";
      pp_expr ppf e;
      fprintf ppf "\n"
    in
    helper pe;
    List.iter helper pes

and no_pars ppf = pp_expr_helper ~ps:false ppf

and maybe_pars ppf = pp_expr_helper ~ps:true ppf

let pp_expr = pp_expr_helper ~ps:true

let pp_value_binding ppf (is_rec, pat, rhs) =
  let () =
    (match is_rec with
     | Parsetree.Recursive -> fprintf ppf "@[<v 2>@[let rec %a "
     | NonRecursive -> fprintf ppf "@[<v 2>@[let %a ")
      pp_pattern
      pat
  in
  match group_lams rhs with
  | args, rhs ->
    List.iter (fprintf ppf "%a@ " pp_pattern) args;
    fprintf ppf "=@ @]@[%a@]@]" no_pars rhs
;;

open Typedtree

let rec pp_typ ppf { typ_desc } =
  match typ_desc with
  | V { binder; _ } -> fprintf ppf "'_%d" binder
  | Prim s -> pp_print_string ppf s
  | Arrow (l, r) -> fprintf ppf "(%a -> %a)" pp_typ l pp_typ r
  | TLink t -> pp_typ ppf t
  | TProd (a, b, ts) ->
    fprintf ppf "@[(%a, %a" pp_typ a pp_typ b;
    List.iter (fprintf ppf ", %a" pp_typ) ts;
    fprintf ppf ")@]"
;;

let pp_scheme ppf = function
  | S (xs, t) -> fprintf ppf "forall %a . %a" Var_set.pp xs pp_typ t
;;

let pp_ls ppf sep pp = function
  | [] -> ()
  | [ x ] -> pp ppf x
  | x :: xs ->
    pp ppf x;
    List.iter
      (fun x ->
         fprintf ppf "%s" sep;
         pp ppf x)
      xs
;;

let rec pp_core_type ppf = function
  | CTVar name -> fprintf ppf "%s" name
  | CTArrow (a, b) ->
    fprintf ppf "(";
    pp_core_type ppf a;
    fprintf ppf " -> ";
    pp_core_type ppf b;
    fprintf ppf ")"
  | CTTuple (a, b, xs) ->
    fprintf ppf "(";
    pp_ls ppf " * " pp_core_type (a :: b :: xs);
    fprintf ppf ")"
  | CTConstr (arg, name) ->
    fprintf ppf "%s (" name;
    pp_core_type ppf arg;
    fprintf ppf ")"
;;

let pp_typedef ppf td =
  fprintf ppf "type";

  (match td.typedef_params with
   | [] -> ()
   | [ x ] -> fprintf ppf " %s " x
   | xs ->
     fprintf ppf " (";
     List.iter (fun name -> fprintf ppf "%s, " name) xs;
     fprintf ppf ")");

  fprintf ppf " %s =" td.typedef_name;

  match td.typedef_kind with
  | TKAlias alias ->
    fprintf ppf " ";
    pp_core_type ppf alias
  | TKVariants (x, xs) ->
    fprintf ppf "\n";
    List.iter
      (fun (name, ct_opt) ->
         match ct_opt with
         | None -> fprintf ppf "| %s\n" name
         | Some ct ->
           fprintf ppf "| %s of " name;
           pp_core_type ppf ct;
           fprintf ppf "\n")
      (x :: xs)
;;

let pp_structure_item ppf = function
  | SLet item -> pp_value_binding ppf item
  | SType (x, xs) -> List.iter (fun item -> pp_typedef ppf item) (x :: xs)
;;

let structure ppf xs =
  List.iter
    (fun x ->
       pp_structure_item ppf x;
       fprintf ppf "\n")
    xs
;;
