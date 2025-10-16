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
  | PVar s -> fprintf ppf "@[%s@]" s
  | PTuple (pa, pb, ps) ->
    fprintf ppf "@[(%a" pp_pattern pa;
    List.iter (fprintf ppf ", %a" pp_pattern) (pb :: ps);
    fprintf ppf ")@]"
  | PAny -> fprintf ppf "@[_@]"
  | PConstruct ("Cons", Some (PTuple (head, tail, []))) ->
    fprintf ppf "@[(%a :: %a)@]" pp_pattern head pp_pattern tail
  | PConstruct ("Nil", None) -> fprintf ppf "[]"
  | PConstruct (name, None) -> fprintf ppf "@[%s@]" name
  | PConstruct (name, Some arg) -> fprintf ppf "@[%s (%a)@]" name pp_pattern arg
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
  | EMatch (e, (pe, pes)) ->
    fprintf ppf "@[<v 2>";
    fprintf ppf "match %a with@ " pp_expr e;
    List.iter
      (fun (p, e) -> fprintf ppf "@[| %a -> %a@]@ " pp_pattern p pp_expr e)
      (pe :: pes);
    fprintf ppf "@]"
  | EConstruct ("Cons", Some (ETuple (head, tail, []))) ->
    fprintf ppf "@[%a :: %a@]" no_pars head maybe_pars tail
  | EConstruct ("Nil", None) -> fprintf ppf "[]"
  | EConstruct (name, None) -> fprintf ppf "%s" name
  | EConstruct (name, Some arg) -> fprintf ppf "@[%s %a@]" name maybe_pars arg

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

let rec pp_core_type ppf = function
  | CTVar name -> fprintf ppf "%s" name
  | CTArrow (ctl, ctr) -> fprintf ppf "(%a -> %a)" pp_core_type ctl pp_core_type ctr
  | CTTuple (ct1, ct2, cts) ->
    fprintf ppf "@[(%a * %a" pp_core_type ct1 pp_core_type ct2;
    List.iter (fprintf ppf " * %a" pp_core_type) cts;
    fprintf ppf ")@]"
  | CTConstr (name, []) -> fprintf ppf "@[%s@]" name
  | CTConstr (name, [ arg ]) -> fprintf ppf "@[(%a) %s@]" pp_core_type arg name
  | CTConstr (name, arg :: args) ->
    fprintf ppf "@[(%a" pp_core_type arg;
    List.iter (fprintf ppf ", %a" pp_core_type) args;
    fprintf ppf ") %s@]" name
;;

let pp_type_params ppf td =
  match td.typedef_params with
  | [] -> fprintf ppf " "
  | [ x ] -> fprintf ppf " %s " x
  | x :: xs ->
    fprintf ppf "@[ (%s" x;
    List.iter (fprintf ppf ", %s") xs;
    fprintf ppf ") @]"
;;

let pp_type_kind ppf td =
  match td.typedef_kind with
  | KAbstract None -> ()
  | KAbstract (Some ct) -> fprintf ppf "@[%a@]" pp_core_type ct
  | KVariants (case, cases) ->
    let pp_case ppf = function
      | name, None -> fprintf ppf "| %s" name
      | name, Some ct -> fprintf ppf "| %s of %a" name pp_core_type ct
    in
    fprintf ppf "@[<v>";
    List.iter (fprintf ppf "@[%a@]@ " pp_case) (case :: cases);
    fprintf ppf "@]"
;;

let pp_type_definition ppf (td, tds) =
  fprintf ppf "@[<v>";
  fprintf
    ppf
    "@[<v 2>@[type%a%s =@]@ @[%a@]@]@ "
    pp_type_params
    td
    td.typedef_name
    pp_type_kind
    td;
  List.iter
    (fun td ->
       fprintf
         ppf
         "@[<v 2>@[and%a%s =@]@ @[%a@]@]@ "
         pp_type_params
         td
         td.typedef_name
         pp_type_kind
         td)
    tds;
  fprintf ppf "@]"
;;

let pp_structure_item ppf = function
  | Parsetree.SValue vb -> pp_value_binding ppf vb
  | Parsetree.SType tds -> pp_type_definition ppf tds
;;

let pp_stru ppf vbs =
  let open Format in
  open_vbox 0;
  pp_print_list
    ~pp_sep:(fun ppf () -> fprintf ppf "@ ")
    (fun ppf -> fprintf ppf "@[%a@]" pp_structure_item)
    ppf
    vbs;
  close_box ()
;;

let value_bindings ppf = List.iter (fun vb -> pp_value_binding ppf vb)
let structure = pp_stru
