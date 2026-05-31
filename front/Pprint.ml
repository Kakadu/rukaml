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
  | PConst_char c -> fprintf ppf "%c" c
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

(** TODOs:
    omit parens in [ let (Some x) = ... ] and dont omit in [ let f (Some x) = ... ]
    omit parens in [ let (x, y) = ... ] and dont omit in [ let f (x, y) = ... ] *)
type pp_pattern_ctx =
  | CtxTuple (* <here>, <here>, <here> *)
  | CtxSingleConstrArg (* Some <here> *)
  | CtxManyConstrArgs (* Some (<here>, <here>, <here>) *)
  | CtxLeftSideFun (* fun <here> -> ... *)
  | CtxLeftSideMatch (* | <here> -> ... *)
  | CtxLeftSideLet (* let <here> = ... *)
  | CtxListBrackets (* [ <here>; <here>; <here> ] *)
  | CtxListSemicolons (* <here> :: <here> *)
  | CtxFree (* omits outer parens *)

let rec pp_pattern ctx ppf = function
  | PAny -> fprintf ppf "_"
  | PUnit -> fprintf ppf "()"
  | PConst x -> pp_const ppf x
  | PVar s -> fprintf ppf "%s" s
  | PConstruct (name, []) -> fprintf ppf "%s" name
  | PTuple (pa, pb, ps) ->
    let fmt : _ format =
      match ctx with
      | CtxTuple
      | CtxLeftSideFun
      | CtxSingleConstrArg
      | CtxManyConstrArgs
      | CtxListSemicolons
      | CtxLeftSideLet -> "(%a)"
      | CtxFree | CtxListBrackets | CtxLeftSideMatch -> "%a"
    in
    let pp_tuple ppf () =
      fprintf ppf "@[%a" (pp_pattern CtxTuple) pa;
      List.iter (fprintf ppf ", %a" (pp_pattern CtxTuple)) (pb :: ps);
      fprintf ppf "@]"
    in
    fprintf ppf fmt pp_tuple ()
  | PConstruct ("::", [ head; tail ]) ->
    let rec aux acc = function
      | PConstruct ("::", [ hd; tl ]) -> aux (hd :: acc) tl
      | PConstruct ("[]", []) ->
        pp_cons_brackets ppf ~pp_item:(pp_pattern CtxListBrackets) head (List.rev acc)
      | _ as exp ->
        pp_cons_semicolons
          ppf
          ~pp_item:(pp_pattern CtxListSemicolons)
          head
          (List.rev (exp :: acc))
    in
    aux [] tail
  | PConstruct (name, [ arg ]) ->
    let fmt : _ format =
      match ctx with
      | CtxFree | CtxTuple | CtxManyConstrArgs | CtxListBrackets | CtxLeftSideMatch ->
        "@[%s %a@]"
      | CtxLeftSideFun | CtxSingleConstrArg | CtxListSemicolons | CtxLeftSideLet ->
        "@[(%s %a)@]"
    in
    fprintf ppf fmt name (pp_pattern CtxSingleConstrArg) arg
  | PConstruct (name, args) ->
    let fmt : _ format =
      match ctx with
      | CtxFree | CtxTuple | CtxManyConstrArgs | CtxListBrackets | CtxLeftSideMatch ->
        "%a"
      | CtxLeftSideFun | CtxSingleConstrArg | CtxListSemicolons | CtxLeftSideLet -> "(%a)"
    in
    let pp_constructor ppf () =
      let pp_sep ppf () = fprintf ppf ", " in
      fprintf ppf "@[%s (" name;
      pp_print_list ~pp_sep (pp_pattern CtxManyConstrArgs) ppf args;
      fprintf ppf ")@]"
    in
    fprintf ppf fmt pp_constructor ()
;;

let pp_flg ppf = function
  | Recursive -> Format.fprintf ppf "rec "
  | NonRecursive -> ()
;;

type pp_expr_ctx =
  | CtxTuple (* <here>, <here>, <here> or Some (<here>, <here>, <here>)*)
  | CtxAfterIf (* if <here> then ... else ... *)
  | CtxAfterThen (* if ... then <here> else ... *)
  | CtxAfterElse (* if ... then ... else <here> *)
  | CtxAppLeft (** f in [ f x ] *)
  | CtxAppRight (** x in [ f x ] or in [ Some x ] *)
  | CtxMatchWith (* match <here> with ... *)
  | CtxRightSideFun (* fun ... -> <here> *)
  | CtxRightSideLet (* let ... = <here> *)
  | CtxRightSideMatch (* | ... -> <here> *)
  | CtxLetBody (* let ... = ... in <here> *)
  | CtxBinop (* <here> + <here> / <here> *)
  | CtxContainer (* [ <here>; <here>; <here> ] or [| <here>; <here>; <here> |] *)
  | CtxFree

let rec pp_expr ctx ppf = function
  | EUnit -> fprintf ppf "()"
  | EConst n -> pp_const ppf n
  | EIf (c, th, el) ->
    let fmt : _ format =
      match ctx with
      | CtxFree
      | CtxAfterElse
      | CtxRightSideFun
      | CtxRightSideLet
      | CtxRightSideMatch
      | CtxLetBody -> "%a"
      | _ -> "(%a)"
    in
    let pp_ite ppf () =
      fprintf ppf "@[<hov>";
      fprintf ppf "@[if %a@ @]" (pp_expr CtxAfterIf) c;
      fprintf ppf "@[then %a@ @]" (pp_expr CtxAfterThen) th;
      fprintf ppf "@[else %a@]" (pp_expr CtxAfterElse) el;
      fprintf ppf "@]"
    in
    fprintf ppf fmt pp_ite ()
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
    let fmt : _ format =
      match ctx with
      | CtxAppLeft | CtxAppRight | CtxBinop -> "(%a %s %a)"
      | _ -> "%a %s %a"
    in
    fprintf ppf fmt (pp_expr CtxBinop) l op (pp_expr CtxBinop) r
  | EApp (l, r) ->
    let fmt : _ format =
      match ctx with
      | CtxAppRight -> "(%a %a)"
      | _ -> "%a %a"
    in
    fprintf ppf fmt (pp_expr CtxAppLeft) l (pp_expr CtxAppRight) r
  | ELam (pat, e) ->
    let fmt : _ format =
      match ctx with
      | CtxAfterElse | CtxFree | CtxLetBody | CtxRightSideFun | CtxRightSideMatch ->
        "@[fun %a ->@[ %a@]"
      | _ -> "@[(fun %a ->@[ %a@])"
    in
    fprintf ppf fmt (pp_pattern CtxLeftSideFun) pat (pp_expr CtxRightSideFun) e
  | ELet (flg, pat, body, in_) ->
    let fmt : _ format =
      match ctx with
      | CtxFree | CtxLetBody | CtxRightSideLet -> "%a"
      | _ -> "(%a)"
    in
    let pp_let ppf () =
      let rec_ =
        match flg with
        | Recursive -> "rec "
        | _ -> ""
      in
      fprintf ppf "@[<v>@[<hv>@[let %s%a " rec_ (pp_pattern CtxLeftSideLet) pat;
      let args, body = group_lams body in
      List.iter (fprintf ppf "%a " (pp_pattern CtxLeftSideLet)) args;
      fprintf ppf "= @]";
      Format.fprintf ppf "@[<2>%a @]@[in @]@]" (pp_expr CtxRightSideLet) body;
      fprintf ppf "@[%a@]" (pp_expr CtxLetBody) in_;
      fprintf ppf "@]"
    in
    fprintf ppf fmt pp_let ()
  | EArray [] -> fprintf ppf "[| |]"
  | EArray r ->
    let pp_sep ppf () = fprintf ppf "; " in
    Format.fprintf ppf "[| %a |]" (pp_print_list ~pp_sep (pp_expr CtxContainer)) r
  | ETuple (h1, h2, hs) ->
    let fmt : _ format =
      match ctx with
      | CtxTuple | CtxBinop | CtxAppLeft | CtxAppRight -> "(%a)"
      | _ -> "%a"
    in
    let pp_tuple ppf () =
      let pp_sep ppf () = fprintf ppf ", " in
      fprintf ppf "@[%a, " (pp_expr CtxTuple) h1;
      pp_print_list ~pp_sep (pp_expr CtxTuple) ppf (h2 :: hs);
      fprintf ppf "@]"
    in
    fprintf ppf fmt pp_tuple ()
  | EMatch (e, (pe, pes)) ->
    let fmt : _ format =
      match ctx with
      | CtxAfterElse | CtxFree | CtxMatchWith | CtxRightSideFun ->
        "@[<v 2>match %a with@ %a@]"
      | _ -> "(@[<v 2>match %a with@ %a@])"
    in
    let pp_cases ppf () =
      let pp_case ppf (p, e) =
        fprintf ppf "@[| %a -> " (pp_pattern CtxLeftSideMatch) p;
        fprintf ppf "%a@]" (pp_expr CtxRightSideMatch) e
      in
      pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf "@ ") pp_case ppf (pe :: pes)
    in
    fprintf ppf fmt (pp_expr CtxMatchWith) e pp_cases ()
  | EConstruct ("[]", []) -> fprintf ppf "[]"
  | EConstruct ("::", [ head; tail ]) ->
    let rec aux acc = function
      | EConstruct ("::", [ hd; tl ]) -> aux (hd :: acc) tl
      | EConstruct ("[]", []) ->
        pp_cons_brackets ppf ~pp_item:(pp_expr CtxContainer) head (List.rev acc)
      | _ as exp ->
        let pars =
          match ctx with
          | CtxAppLeft | CtxAppRight | CtxBinop -> true
          | _ -> false
        in
        pp_cons_semicolons
          ppf
          ~pars
          ~pp_item:(pp_expr CtxBinop)
          head
          (List.rev (exp :: acc))
    in
    aux [] tail
  | EConstruct (name, []) -> fprintf ppf "%s" name
  | EConstruct (name, [ arg ]) ->
    let fmt : _ format =
      match ctx with
      | CtxAppLeft | CtxAppRight -> "(%s %a)"
      | _ -> "%s %a"
    in
    fprintf ppf fmt name (pp_expr CtxAppRight) arg
  | EConstruct (name, args) ->
    let fmt : _ format =
      match ctx with
      | CtxAppLeft | CtxAppRight -> "(%a)"
      | _ -> "%a"
    in
    let pp_constructor ppf () =
      let pp_sep ppf () = fprintf ppf ", " in
      fprintf ppf "@[%s (" name;
      pp_print_list ~pp_sep (pp_expr CtxTuple) ppf args;
      fprintf ppf ")@]"
    in
    fprintf ppf fmt pp_constructor ()
;;

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

let rec pp_core_type ppf = function
  | CTVar name -> fprintf ppf "%s" name
  | CTArrow (ctl, ctr) -> fprintf ppf "(%a -> %a)" pp_core_type ctl pp_core_type ctr
  | CTTuple (ct1, ct2, cts) ->
    fprintf ppf "@[(%a * %a" pp_core_type ct1 pp_core_type ct2;
    List.iter (fprintf ppf " * %a" pp_core_type) cts;
    fprintf ppf ")@]"
  | CTConstr (name, []) -> fprintf ppf "@[%s@]" name
  | CTConstr (name, [ CTVar var_name ]) -> fprintf ppf "@[%s %s@]" var_name name
  | CTConstr (name, arg :: args) ->
    fprintf ppf "@[(%a" pp_core_type arg;
    List.iter (fprintf ppf ", %a" pp_core_type) args;
    fprintf ppf ") %s@]" name
;;

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
  | KAbstract None -> ()
  | KAbstract (Some ct) -> fprintf ppf " = %a" pp_core_type ct
  | KVariants (case, cases) ->
    let pp_case ppf = function
      | name, None -> fprintf ppf "| %s" name
      | name, Some ct -> fprintf ppf "| %s of %a" name pp_core_type ct
    in
    fprintf ppf " =@ ";
    List.iter (fprintf ppf "%a@ " pp_case) (case :: cases)
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
  | Parsetree.SValue vb -> pp_value_binding ppf vb
  | Parsetree.SType tds -> pp_type_declaration ppf tds
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
