open Parsetree
open Typedtree
open Format

type pp_typ_ctx =
  | CArrow_left
  | CArrow_right
  | CTuple

let pp_typ_hum =
  let open Format in
  let rec pp_typ ctx ppf t =
    match t.typ_desc with
    | V { binder; _ } -> fprintf ppf "'_%d" binder
    | Weak n -> fprintf ppf "'_weak%d" n
    | TLink ty -> pp_typ ctx ppf ty
    | Arrow (l, r) ->
      let fmt : _ format =
        match ctx with
        | CArrow_right -> "%a -> %a"
        | CArrow_left | CTuple -> "(%a -> %a)"
      in
      fprintf ppf fmt (pp_typ CArrow_left) l (pp_typ CArrow_right) r
    | TProd (a, b, ts) ->
      let fmt : _ format =
        match ctx with
        | CArrow_left | CArrow_right -> "%a"
        | CTuple -> "(%a)"
      in
      fprintf
        ppf
        fmt
        (fun ppf () ->
           fprintf ppf "@[%a * %a" (pp_typ CTuple) a (pp_typ CTuple) b;
           List.iter (fprintf ppf " * %a" (pp_typ CTuple)) ts;
           fprintf ppf "@]")
        ()
    | TConstr ([], name) -> fprintf ppf "%s" name
    | TConstr (params, name) ->
      (* TODO : fix this *)
      fprintf
        ppf
        "(%a) %s"
        (pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf ", ") (pp_typ CTuple))
        params
        name
  in
  pp_typ CArrow_right
;;

let rec pp_pattern ppf = function
  | Tpat_unit -> fprintf ppf "()"
  | Tpat_const (PConst_int n) -> fprintf ppf "%d" n
  | Tpat_const (PConst_bool b) -> fprintf ppf "%b" b
  | Tpat_var id -> Ident.pp ppf id
  | Tpat_tuple (h1, h2, []) -> fprintf ppf "(%a, %a)" pp_pattern h1 pp_pattern h2
  | Tpat_tuple (h1, h2, rest) ->
    fprintf
      ppf
      "(%a, %a, %a)"
      pp_pattern
      h1
      pp_pattern
      h2
      (pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf " ") pp_pattern)
      rest
  | Tpat_any -> fprintf ppf "_"
  | Tpat_constr (name, _id, None) -> fprintf ppf "%s" name
  | Tpat_constr (name, _id, Some patt) -> fprintf ppf "%s %a" name pp_pattern patt
;;

let pp_expr =
  let open Format in
  let rec extract_lambdas acc e =
    match e with
    | TLam (pat, body, _) -> extract_lambdas (pat :: acc) body
    | body -> List.rev acc, body
  in
  let rec expr_gen ?(pars = true) ppf = function
    | TUnit -> fprintf ppf "()"
    | TConst c -> Pprint.pp_const ppf c
    | TVar (name, _, _) -> fprintf ppf "%s" name
    | TIf (cond, th, el, _) ->
      fprintf ppf "(if %a then %a else %a)" expr_no cond expr_no th expr_no el
    | TArray (xs, _) ->
      fprintf ppf "[|";
      (match xs with
       | [] -> ()
       | h :: tl ->
         expr_gen ~pars:false ppf h;
         List.iter
           (fun x ->
              fprintf ppf "; ";
              expr_gen ~pars:false ppf x)
           tl);
      fprintf ppf "|]"
    | TLam (pat, e, _) ->
      (match extract_lambdas [ pat ] e with
       | [], _ -> failwith "TODO: Should not happend. Rewrite!"
       | ps, e ->
         if pars then fprintf ppf "(";
         fprintf ppf "fun ";
         List.iter (fun name -> fprintf ppf "%a " pp_pattern name) ps;
         fprintf ppf "-> %a" expr_no e;
         if pars then fprintf ppf ")")
    | TApp (TApp (TVar ("+", _, _), l, _), r, _) ->
      fprintf ppf (if pars then "(%a + %a)" else "%a + %a") expr l expr r
    | TApp (TApp (TVar ("*", _, _), l, _), r, _) ->
      fprintf ppf (if pars then "(%a * %a)" else "%a * %a") expr l expr r
      (* fprintf ppf "(%a * %a)" expr l expr r *)
    | TApp (TApp (TVar ("-", _, _), l, _), r, _) ->
      fprintf ppf (if pars then "(%a - %a)" else "%a - %a") expr l expr r
      (* fprintf ppf "(%a - %a)" expr l expr r *)
    | TApp (TApp (TVar ("=", _, _), l, _), r, _) ->
      fprintf ppf (if pars then "(%a = %a)" else "%a = %a") expr l expr r
      (* fprintf ppf "(%a = %a)" expr l expr r *)
    | TApp (l, r, _) -> fprintf ppf (if pars then "(%a %a)" else "%a %a") expr l expr r
    | TLet (Parsetree.Recursive, pat, S (_vars, ty), rhs, wher) ->
      fprintf
        ppf
        "let rec %a : %a = %a in %a"
        pp_pat
        pat
        pp_typ
        ty
        expr_no
        rhs
        expr_no
        wher
    | TLet (NonRecursive, pat, S (_vars, ty), rhs, wher) ->
      fprintf
        ppf
        "@[let %a : %a = %a in@]@,%a"
        pp_pat
        pat
        pp_typ
        ty
        expr_no
        rhs
        expr_no
        wher
    | TTuple (a, b, es, _) ->
      fprintf ppf "@[(%a, %a" expr a expr b;
      List.iter (fprintf ppf ", %a" expr_no) es;
      fprintf ppf ")@]"
    | TMatch (e, (case, cases), _) ->
      let pp_match ppf () =
        let pp_case ppf (patt, expr) =
          fprintf ppf "| %a -> %a@ " pp_pattern patt expr_no expr
        in
        fprintf ppf "match %a with@ " expr_no e;
        fprintf ppf "%a" (fun ppf -> pp_print_list pp_case ppf) (case :: cases)
      in
      fprintf ppf "@[<v 2>%a@]" pp_match ()
    | TConstruct (name, _id, None, _ty) -> fprintf ppf "%s" name
    | TConstruct (name, _id, Some arg, _ty) -> fprintf ppf "%s %a" name expr arg
  and pp_typ = pp_typ_hum
  and pp_pat ppf s = fprintf ppf "%a" pp_pattern s
  and expr ppf = expr_gen ~pars:true ppf
  and expr_no ppf = expr_gen ~pars:false ppf in
  fun ?(pars = false) ppf e -> fprintf ppf "@[<v>%a@]" (expr_gen ~pars) e
;;

let pp_hum = pp_expr ~pars:false

let pp_vb_hum ppf { tvb_flag; tvb_pat; tvb_body; tvb_typ } =
  fprintf
    ppf
    "@[<v 2>@[let %s%a: @[%a@] =@]@,@[%a@]@]"
    (match tvb_flag with
     | Recursive -> "rec "
     | NonRecursive -> "")
    pp_pattern
    tvb_pat
    pp_typ_hum
    (match tvb_typ with
     | S (_, typ) -> typ)
    (fun ppf e -> pp_hum ppf e)
    tvb_body
;;

let pp_td_hum ppf td =
  fprintf ppf "type (%a) %s" pp_binder_set td.tty_params td.tty_name;
  match td.tty_kind with
  | Tty_abstract None -> ()
  | Tty_abstract (Some ty) -> fprintf ppf " = %a@ " pp_typ_hum ty
  | Tty_variants variants ->
    let pp_variant ppf = function
      | name, None -> fprintf ppf "| %s" name
      | name, Some ty -> fprintf ppf "| %s of %a" name pp_typ_hum ty
    in
    fprintf ppf " =@ ";
    pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf "@ ") pp_variant ppf variants
;;

let pp_constructor_entry ppf (entry : TypeEnv.constructor_entry) =
  fprintf ppf "%s" entry.constr_name;
  match entry.constr_arg_ty with
  | None -> ()
  | Some ty -> fprintf ppf " (%a)" pp_ty ty
;;

let iter_idents = Ident.Ident_map.iter_idents

let pp_stru_item ppf = function
  | Tstr_type td -> pp_td_hum ppf td
  | Tstr_value vb -> pp_vb_hum ppf vb
;;

let pp_stru ppf stru =
  open_vbox 0;
  pp_print_list
    ~pp_sep:(fun ppf () -> fprintf ppf "@ ")
    (fun ppf (_env, item) -> fprintf ppf "%a@ " pp_stru_item item)
    ppf
    stru;
  close_box ()
;;

(* inferencer testing stuff *)

let pp_env_hum ppf (env : Typedtree.TypeEnv.t) =
  fprintf ppf "values:@ ";
  iter_idents ~f:(fun _id scheme -> fprintf ppf "%a" pp_scheme scheme) env.env_values;
  fprintf ppf "types:@ ";
  iter_idents ~f:(fun _id td -> pp_td_hum ppf td) env.env_types;
  fprintf ppf "constructors:@ ";
  iter_idents ~f:(fun _id constr -> pp_constructor_entry ppf constr) env.env_constructors
;;

let pp_stru_verbose_env ppf (stru : Typedtree.structure) =
  open_vbox 0;
  pp_print_list
    ~pp_sep:(fun ppf () -> fprintf ppf "@ ")
    (fun ppf (env, item) -> fprintf ppf "%a@ %a@ " pp_env_hum env pp_stru_item item)
    ppf
    stru;
  close_box ()
;;
