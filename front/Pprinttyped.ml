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
    | TConstr ([ param ], name) -> fprintf ppf "%a %s" (pp_typ ctx) param name
    | TConstr (params, name) ->
      fprintf ppf "(";
      let pp_sep ppf () = fprintf ppf ", " in
      pp_print_list (pp_typ CTuple) ~pp_sep ppf params;
      fprintf ppf ") %s" name
  in
  pp_typ CArrow_right
;;

let cons_ident = Typedtree.TypeEnv.TypeList.constr_cons.constr_ident
let nil_ident = Typedtree.TypeEnv.TypeList.constr_nil.constr_ident

let rec pp_pattern ppf = function
  | Tpat_unit -> fprintf ppf "()"
  | Tpat_const (PConst_int n) -> fprintf ppf "%d" n
  | Tpat_const (PConst_bool b) -> fprintf ppf "%b" b
  | Tpat_const (PConst_char c) -> fprintf ppf "%c" c
  | Tpat_const (PConst_string s) -> fprintf ppf "\"%s\"" s
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
  | Tpat_constr (ident, []) -> fprintf ppf "%s" ident.hum_name
  | Tpat_constr (ident, [ head; tail ])
  (* syntactic sugar for lists *)
    when Ident.equal ident cons_ident ->
    let rec aux acc = function
      | Tpat_constr (ident, [ hd; tl ]) when Ident.equal ident cons_ident ->
        aux (hd :: acc) tl
      | Tpat_constr (ident, []) when Ident.equal ident nil_ident ->
        Pprint.pp_cons_brackets ppf head ~pp_item:pp_pattern (List.rev acc)
      | _ as exp ->
        Pprint.pp_cons_semicolons ppf ~pp_item:pp_pattern head (List.rev (exp :: acc))
    in
    aux [] tail
  | Tpat_constr (ident, [ arg ]) -> fprintf ppf "%s %a" ident.hum_name pp_pattern arg
  | Tpat_constr (ident, args) ->
    fprintf ppf "@[%s (" ident.hum_name;
    pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf ", ") pp_pattern ppf args;
    fprintf ppf ")@]"
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
    | TVar (name, _, _, _) -> fprintf ppf "%s" name
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
    | TApp (TApp (TVar ("+", _, _, _), l, _), r, _) ->
      fprintf ppf (if pars then "(%a + %a)" else "%a + %a") expr l expr r
    | TApp (TApp (TVar ("*", _, _, _), l, _), r, _) ->
      fprintf ppf (if pars then "(%a * %a)" else "%a * %a") expr l expr r
      (* fprintf ppf "(%a * %a)" expr l expr r *)
    | TApp (TApp (TVar ("-", _, _, _), l, _), r, _) ->
      fprintf ppf (if pars then "(%a - %a)" else "%a - %a") expr l expr r
      (* fprintf ppf "(%a - %a)" expr l expr r *)
    | TApp (TApp (TVar ("=", _, _, _), l, _), r, _) ->
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
          fprintf ppf "| %a -> %a" pp_pattern patt expr_no expr
        in
        let pp_sep ppf () = fprintf ppf "@ " in
        fprintf ppf "match %a with@ " expr_no e;
        fprintf ppf "%a" (fun ppf -> pp_print_list ~pp_sep pp_case ppf) (case :: cases)
      in
      if pars
      then fprintf ppf "(@[<v 2>%a@])" pp_match ()
      else fprintf ppf "@[<v 2>%a@]" pp_match ()
    | TConstruct (ident, [], _ty) -> fprintf ppf "%s" ident.hum_name
    | TConstruct (ident, [ head; tail ], _ty)
    (* syntactic sugar for lists *)
      when Ident.equal ident cons_ident ->
      let rec aux ppf acc = function
        | TConstruct (ident, [ hd; tl ], _) when Ident.equal ident cons_ident ->
          aux ppf (hd :: acc) tl
        | TConstruct (ident, [], _) when Ident.equal ident nil_ident ->
          Pprint.pp_cons_brackets ppf head ~pp_item:expr_no (List.rev acc)
        | _ as exp ->
          Pprint.pp_cons_semicolons ppf ~pp_item:expr ~pars head (List.rev (exp :: acc))
      in
      aux ppf [] tail
    | TConstruct (ident, args, _ty) ->
      fprintf ppf (if pars then "@[(" else "@[");
      fprintf ppf "%s (" ident.hum_name;
      pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf ", ") expr_no ppf args;
      fprintf ppf ")";
      fprintf ppf (if pars then ")@]" else "@]")
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
  let pp_binder ppf binder = fprintf ppf "'_%d" binder in
  let pp_params ppf params =
    match List.of_seq (Var_set.to_seq params) with
    | [] -> ()
    | [ x ] -> fprintf ppf " %a " pp_binder x
    | xs ->
      let pp_sep ppf () = fprintf ppf ", " in
      fprintf ppf " (%a) " (pp_print_list ~pp_sep pp_binder) xs
  in
  fprintf ppf "@[<v 2>";
  fprintf ppf "type%a%s" pp_params td.tty_params td.tty_ident.hum_name;
  (match td.tty_kind with
   | Ttype_variants variants ->
     let pp_variant ppf variant =
       match variant.constr_args with
       | [] -> fprintf ppf "| %s" variant.constr_ident.hum_name
       | args ->
         fprintf ppf "| %s of " variant.constr_ident.hum_name;
         pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf " * ") pp_typ_hum ppf args
     in
     fprintf ppf " =@ ";
     pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf "@ ") pp_variant ppf variants
   | Ttype_abstract ->
     (match td.tty_manifest with
      | Some ty -> fprintf ppf " = %a@ " pp_typ_hum ty
      | None -> ()));
  fprintf ppf "@]"
;;

let iter_idents = Ident.Ident_map.iter_idents

let pp_stru_item ppf = function
  | Tstr_type td -> pp_td_hum ppf td
  | Tstr_value vb -> pp_vb_hum ppf vb
;;

let pp_stru ppf stru =
  open_vbox 0;
  let pp_sep ppf () = fprintf ppf "@." in
  let pp_item ppf item = fprintf ppf "%a" pp_stru_item item in
  pp_print_list ~pp_sep pp_item ppf stru;
  close_box ()
;;
