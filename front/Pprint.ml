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

let pp_const ppf = function
  | PConst_bool b -> fprintf ppf "%b" b
  | PConst_int n -> fprintf ppf "%d" n
  | PConst_char c -> fprintf ppf "\'%c\'" c
  | PConst_string s -> fprintf ppf "\"%s\"" s
;;

let pp_cons_brackets ppf ~pp_item hd tl =
  fprintf ppf "[@[ %a" pp_item hd;
  List.iter (fun item -> fprintf ppf "; %a" pp_item item) tl;
  fprintf ppf " ]@]"
;;

let pp_cons_semicolons ppf ~pp_item ?(pars = false) hd tl =
  if pars then fprintf ppf "(";
  fprintf ppf "%a" pp_item hd;
  List.iter (fun item -> fprintf ppf " :: %a" pp_item item) tl;
  if pars then fprintf ppf ")"
;;

let rec pp_pattern ppf = function
  | PUnit -> fprintf ppf "()"
  | PConst x -> pp_const ppf x
  | PVar s -> fprintf ppf "@[%s@]" s
  | PTuple (pa, pb, ps) ->
    fprintf ppf "@[(%a" pp_pattern pa;
    List.iter (fprintf ppf ", %a" pp_pattern) (pb :: ps);
    fprintf ppf ")@]"
  | PAny -> fprintf ppf "@[_@]"
  | PConstruct ("[]", []) -> fprintf ppf "[]"
  | PConstruct ("::", [ head; tail ]) ->
    let rec aux acc = function
      | PConstruct ("::", [ hd; tl ]) -> aux (hd :: acc) tl
      | PConstruct ("[]", []) ->
        pp_cons_brackets ppf ~pp_item:pp_pattern head (List.rev acc)
      | _ as exp ->
        pp_cons_semicolons ppf ~pp_item:pp_pattern head (List.rev (exp :: acc))
    in
    aux [] tail
  | PConstruct (name, []) -> fprintf ppf "@[%s@]" name
  | PConstruct (name, args) ->
    fprintf ppf "@[%s (" name;
    pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf ", ") pp_pattern ppf args;
    fprintf ppf ")@]"
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
  | EApp (EApp (EVar ("||" as op), l), r)
  | EApp (EApp (EVar ("&&" as op), l), r)
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
  | EArray [] ->
    (* an empty array is indistinguishable from an empty string *)
    fprintf ppf "[| |]"
  | EArray r ->
    let rec aux acc = function
      (* pretty-prints arrays when they are strings. in most cases it works well but it slows down weird incorrect programs *)
      | [] ->
        (* prints array as string literal *)
        fprintf ppf "\"%s\"" (Base.String.of_list (List.rev acc))
      | EConst (PConst_char ch) :: tail ->
        (* collects chars *)
        aux (ch :: acc) tail
      | _ ->
        (* realizing that the array is not a string literal, it prints it as usual *)
        let pp_sep ppf () = fprintf ppf "; " in
        Format.fprintf ppf "[| %a |]" (pp_print_list ~pp_sep no_pars) r
    in
    aux [] r
  | ETuple (h1, h2, hs) ->
    fprintf ppf "@[(%a, " no_pars h1;
    Format.fprintf
      ppf
      "%a"
      (pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf ", ") no_pars)
      (h2 :: hs);
    fprintf ppf ")@]"
  | EMatch (e, (pe, pes)) ->
    let pp_cases ppf () =
      let pp_case ppf (p, e) = fprintf ppf "@[| %a -> %a@]" pp_pattern p maybe_pars e in
      pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf "@ ") pp_case ppf (pe :: pes)
    in
    if ps
    then fprintf ppf "@[<v 2>(match %a with@ %a)@]" no_pars e pp_cases ()
    else fprintf ppf "@[<v 2>match %a with@ %a@]" no_pars e pp_cases ()
  | EConstruct ("[]", []) -> fprintf ppf "[]"
  | EConstruct ("::", [ head; tail ]) ->
    let rec aux acc = function
      | EConstruct ("::", [ hd; tl ]) -> aux (hd :: acc) tl
      | EConstruct ("[]", []) -> pp_cons_brackets ppf ~pp_item:no_pars head (List.rev acc)
      | _ as exp ->
        pp_cons_semicolons ppf ~pars:ps ~pp_item:maybe_pars head (List.rev (exp :: acc))
    in
    aux [] tail
  | EConstruct (name, []) -> fprintf ppf "%s" name
  | EConstruct (name, args) ->
    fprintf ppf "@[%s (" name;
    pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf ", ") maybe_pars ppf args;
    fprintf ppf ")@]"

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
  | Weak n -> fprintf ppf "'_weak%d" n
  | Arrow (l, r) -> fprintf ppf "(%a -> %a)" pp_typ l pp_typ r
  | TLink t -> pp_typ ppf t
  | TProd (a, b, ts) ->
    fprintf ppf "@[(%a, %a" pp_typ a pp_typ b;
    List.iter (fprintf ppf ", %a" pp_typ) ts;
    fprintf ppf ")@]"
  | TConstr ([], name) -> fprintf ppf "%s" name
  | TConstr ([ ty ], name) -> fprintf ppf "@[%a %s@]" pp_typ ty name
  | TConstr (tys, name) ->
    pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf ", ") pp_typ ppf tys;
    fprintf ppf " %s" name
;;

let pp_scheme ppf = function
  | S (xs, t) -> fprintf ppf "forall %a . %a" Var_set.pp xs pp_typ t
;;

type pp_core_type_ctx =
  | CtxLeftSideArrow (* <here> -> ... *)
  | CtxRightSideArrow (* ... -> <here> *)
  | CtxSingleTypeParam (* <here> list *)
  | CtxManyTypeParam (* (<here>, <here>) map *)
  | CtxTuple (* <here> * <here> * <here> *)
  | CtxOmit (* omit outer parens *)
  | CtxDontOmit (* do not omit outer parens *)

let rec pp_core_type ctx ppf = function
  | Ptyp_var name -> fprintf ppf "%s" name
  | Ptyp_arrow (ctl, ctr) ->
    let fmt : _ format =
      match ctx with
      | CtxLeftSideArrow | CtxSingleTypeParam | CtxTuple | CtxDontOmit -> "(%a -> %a)"
      | CtxRightSideArrow | CtxManyTypeParam | CtxOmit -> "%a -> %a"
    in
    fprintf
      ppf
      fmt
      (pp_core_type CtxLeftSideArrow)
      ctl
      (pp_core_type CtxRightSideArrow)
      ctr
  | Ptyp_tuple (ct1, ct2, cts) ->
    let fmt : _ format =
      match ctx with
      | CtxSingleTypeParam | CtxTuple | CtxDontOmit -> "(%a)"
      | CtxRightSideArrow | CtxLeftSideArrow | CtxManyTypeParam | CtxOmit -> "%a"
    in
    let pp_tuple ppf () =
      fprintf ppf "@[%a * %a" (pp_core_type CtxTuple) ct1 (pp_core_type CtxTuple) ct2;
      List.iter (fprintf ppf " * %a" (pp_core_type CtxTuple)) cts;
      fprintf ppf "@]"
    in
    fprintf ppf fmt pp_tuple ()
  | Ptyp_constr (name, []) -> fprintf ppf "@[%s@]" name
  | Ptyp_constr (name, [ arg ]) ->
    fprintf ppf "@[%a %s@]" (pp_core_type CtxSingleTypeParam) arg name
  | Ptyp_constr (name, arg :: args) ->
    fprintf ppf "@[(%a" (pp_core_type CtxManyTypeParam) arg;
    List.iter (fprintf ppf ", %a" (pp_core_type CtxManyTypeParam)) args;
    fprintf ppf ") %s@]" name
;;

let pp_core_type ~pars = pp_core_type (if pars then CtxDontOmit else CtxOmit)

let pp_type_params ppf td =
  match td.pty_params with
  | [] -> fprintf ppf " "
  | [ x ] -> fprintf ppf " %s " x
  | x :: xs ->
    fprintf ppf "@[ (%s" x;
    List.iter (fprintf ppf ", %s") xs;
    fprintf ppf ") @]"
;;

let pp_type_kind ppf td =
  match td.pty_kind with
  | Ptype_variant (case, cases) ->
    let pp_case ppf = function
      | name, [] -> fprintf ppf "| %s" name
      | name, args ->
        fprintf ppf "| %s of " name;
        pp_print_list
          ~pp_sep:(fun ppf () -> fprintf ppf " * ")
          (pp_core_type ~pars:true)
          ppf
          args
    in
    fprintf ppf " =@ ";
    List.iter (fprintf ppf "%a@ " pp_case) (case :: cases)
  | Ptype_abstract ->
    (match td.pty_manifest with
     | None -> ()
     | Some ct -> fprintf ppf " = %a" (pp_core_type ~pars:false) ct)
;;

let pp_type_declaration ppf (td, tds) =
  let aux ppf td ~keyword =
    fprintf ppf "@[<v 2>";
    fprintf ppf "%s" keyword;
    pp_type_params ppf td;
    fprintf ppf "%s" td.pty_name;
    pp_type_kind ppf td;
    fprintf ppf "@]@ "
  in
  open_vbox 0;
  aux ppf ~keyword:"type" td;
  Base.List.iter ~f:(aux ppf ~keyword:"and") tds;
  close_box ()
;;

let pp_structure_item ppf = function
  | Parsetree.Pstr_value vb -> pp_value_binding ppf vb
  | Parsetree.Pstr_type tds -> pp_type_declaration ppf tds
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
