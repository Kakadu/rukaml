open Frontend
open Frontend.Ident
open Frontend.Parsetree
(* Syntax of CPS language *)

let list_fold_map_k f =
  let rec helper lst k =
    match lst with
    | [] -> k []
    | hd :: tl -> f hd (fun hd' -> helper tl (fun tl' -> k (hd' :: tl')))
  in
  helper
;;

let tuple_fold_map_k f el1 el2 els k =
  f el1 (fun el1' ->
    f el2 (fun el2' -> list_fold_map_k f els (fun els' -> k el1' el2' els')))
;;

module Lang (Args : sig
    type 'b t

    val to_cons : 'b t -> 'b * 'b list
  end) =
struct
  type var = ident
  type 'a tuple = 'a * 'a * 'a list
  type 'a safe_binop = var * 'a * 'a (* invarint:  division by zero isn't possible *)

  type pat =
    | CPVar of var
    | CPTuple of pat * pat * pat list

  type p =
    | Call of triv * triv Args.t * cont
    | Ret of cont * triv
    | CIf of triv * p * p
    | Let of rec_flag * pat * triv * p
    | Primop of pat * var * triv * triv list * p
      (* primop constructor conisists of param to bound primop result, operation, first op's arg, other op's args, and wh  *)
    | Letc of var * cont * p

  and cont =
    | Cont of pat * p
    | CVar of var
    | HALT

  and triv =
    | Lam of pat Args.t * var * p
      (* lam contsructor is a triple: params, a cont-param, and a body*)
    | TSafeBinop of triv safe_binop
    | UVar of var
    | TConst of const
    | TTuple of triv tuple
    | TUnit

  type cps_vb = rec_flag * pat * p

  let cps_vb_to_parsetree_vb (rec_flag, pat, p) =
    let rec helper_pat pat k' =
      match pat with
      | CPVar i -> k' (PVar i.hum_name)
      | CPTuple (pat1, pat2, pats) ->
        let k ptrn1 ptrn2 ptrns = k' (PTuple (ptrn1, ptrn2, ptrns)) in
        tuple_fold_map_k helper_pat pat1 pat2 pats k
    in
    let rec helper_triv t k' =
      let open Args in
      match t with
      | TUnit -> k' EUnit
      | TTuple (t1, t2, tt) ->
        tuple_fold_map_k helper_triv t1 t2 tt (fun e1 e2 ee -> k' (ETuple (e1, e2, ee)))
      | UVar i -> k' (EVar i.hum_name)
      | TConst d -> k' (EConst d)
      | Lam (pats, c, p) ->
        let pat, pats = to_cons pats in
        multiparam_lam pat pats c p k'
      | TSafeBinop (op, t1, t2) ->
        helper_triv t1 (fun e1 ->
          helper_triv t2 (fun e2 ->
            let eop = evar op.hum_name in
            let res = eapp eop [ e1; e2 ] in
            k' res))
    and helper_cont cont k' =
      match cont with
      | Cont (pat, p) ->
        helper_pat pat (fun ptrn -> helper_p p (fun e -> k' (ELam (ptrn, e))))
      | CVar v -> k' (EVar v.hum_name)
      | HALT -> k' (ELam (PVar "x", EVar "x"))
    and helper_p p k =
      let open Args in
      match p with
      | Call (t1, tt, c) ->
        let t2, tt = to_cons tt in
        multiarg_app t1 t2 tt c k
      | Ret (HALT, t) -> helper_triv t k
      | Ret (c, t) ->
        helper_cont c (fun e1 -> helper_triv t (fun e2 -> k (EApp (e1, e2))))
      | CIf (t, p1, p2) ->
        helper_triv t (fun e1 ->
          helper_p p1 (fun e2 -> helper_p p2 (fun e3 -> k (EIf (e1, e2, e3)))))
      | Letc (v, c, p) ->
        helper_cont c (fun e1 ->
          helper_p p (fun e2 ->
            let res = elet (PVar v.hum_name) e1 e2 in
            k res))
      | Let (rec_flag, pat, t, p) ->
        helper_pat pat (fun ptrn ->
          helper_triv t (fun e1 ->
            helper_p p (fun e2 -> k (ELet (rec_flag, ptrn, e1, e2)))))
      | Primop (pat, f, t, tt, p) ->
        (fun ptrn ->
          list_fold_map_k helper_triv (t :: tt) (fun ee ->
            helper_p p (fun e2 ->
              let body = eapp (EVar f.hum_name) ee in
              k (ELet (rec_flag, ptrn, body, e2)))))
        |> helper_pat pat
    and multiarg_app t1 t2 tt c k' =
      let rec helper e1 k' = function
        | [] -> helper_cont c (fun e2 -> k' (EApp (e1, e2)))
        | t :: tt -> helper_triv t (fun e2 -> helper (EApp (e1, e2)) k' tt)
      in
      helper_triv t1 (fun e1 -> helper e1 k' (t2 :: tt))
    and multiparam_lam pat pats i p k' =
      let rec helper pats k'' =
        match pats with
        | [] ->
          let ptrn = PVar i.hum_name in
          helper_p p (fun b -> k'' (ELam (ptrn, b)))
        | pat :: pats ->
          helper_pat pat (fun ptrn -> helper pats (fun b -> k'' (ELam (ptrn, b))))
      in
      helper (pat :: pats) k'
    in
    helper_pat pat (fun ptrn -> helper_p p (fun e -> rec_flag, ptrn, e))
  ;;

  open Format

  let cons_uncurry (hd, tl) = hd :: tl

  let rec pp_pat ppf = function
    | CPVar v -> Frontend.Ident.pp ppf v
    | CPTuple (pat1, pat2, pats) ->
      fprintf ppf "@[(%a" pp_pat pat1;
      List.iter (fprintf ppf ", %a" pp_pat) (pat2 :: pats);
      fprintf ppf ")@]"
  ;;

  let pp_list ppf = pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf " ") ppf

  let rec pp_cont ppf = function
    | HALT -> fprintf ppf "@[%s@]" "(fun x -> x)"
    | Cont (pat, p) -> fprintf ppf "@[(fun %a ->@[ %a@])@]" pp_pat pat pp_p p
    | CVar v -> Frontend.Ident.pp ppf v

  and pp_triv ?(ps = true) ppf =
    let open Frontend in
    let open Args in
    function
    | Lam (pats, k, b) ->
      let pats = cons_uncurry @@ to_cons pats in
      fprintf ppf "@[<hv 2>";
      fprintf ppf "@[(fun %a %a -> @]@," pp_pats pats Ident.pp k;
      fprintf ppf "@[%a@])" pp_p b;
      fprintf ppf "@]"
    | TSafeBinop (op, l, r) when ANF.is_infix_binop op.hum_name ->
      pp_binop ppf (ps, op, l, r)
    | UVar v -> Ident.pp ppf v
    | TConst c -> Pprint.pp_const ppf c
    | TTuple (t1, t2, tt) ->
      fprintf ppf "@[(%a, " no_pars t1;
      let pp_sep = fun ppf () -> fprintf ppf ", " in
      Format.fprintf ppf "%a" (pp_print_list ~pp_sep no_pars) (t2 :: tt);
      fprintf ppf ")@]"
    | TUnit -> fprintf ppf "()"
    | TSafeBinop _ -> failwith "not a binop in TSafeBinop"

  and pp_p ppf =
    let open Frontend in
    let open Args in
    function
    | Call (f, aa, k) ->
      let aa = cons_uncurry @@ to_cons aa in
      (* let args = pp_list maybe_pars in *)
      fprintf ppf "@[<hv 2>";
      fprintf ppf "@[%a @]@," maybe_pars f;
      List.iter (fprintf ppf "@[%a @]@," maybe_pars) aa;
      fprintf ppf "%a@]@," pp_cont k
    | Ret (k, a) -> fprintf ppf "@[<hv>%a %a@]" pp_cont k maybe_pars a
    | CIf (c, th, el) ->
      fprintf
        ppf
        "@[<hov>@[if %a@ @]@,@[then %a@ @]@,@[else %a@]@]"
        no_pars
        c
        pp_p
        th
        pp_p
        el;
      ()
    | Let (rec_flag, pat, Lam (pats', k, b), wh) ->
      let rec_ =
        match rec_flag with
        | Recursive -> "rec "
        | _ -> ""
      in
      let pats' = cons_uncurry @@ to_cons pats' in
      let open Ident in
      fprintf
        ppf
        "@[<v>@[<hv 2>@[let %s%a %a %a =@] @,"
        rec_
        pp_pat
        pat
        pp_pats
        pats'
        pp
        k;
      fprintf ppf "@[<2>%a @]@,@[in @]@]@," pp_p b;
      fprintf ppf "@[%a@]@]" pp_p wh
    | Let (rec_flag, pat, b, wh) ->
      let rec_ =
        match rec_flag with
        | Recursive -> "rec "
        | _ -> ""
      in
      fprintf ppf "@[<v>@[<hv>@[let %s%a =@] " rec_ pp_pat pat;
      fprintf ppf "@[<2>%a @]@[in @]@]" no_pars b;
      fprintf ppf "@[%a@]@]" pp_p wh
    | Primop (pat, op, l, [ r ], wh) when ANF.is_infix_binop op.hum_name ->
      fprintf ppf "@[<v>@[<hv>@[let %a =@] " pp_pat pat;
      fprintf ppf "@[<2>%a @]@[in @]@]" pp_binop (false, op, l, r);
      fprintf ppf "@[%a@]@]" pp_p wh
    | Letc (v, Cont (pat, p), wh) ->
      fprintf ppf "@[<v>@[<hv>@[let %a %a =@] " Ident.pp v pp_pat pat;
      fprintf ppf "@[<2>%a @]@[in @]@]" pp_p p;
      fprintf ppf "@[%a@]@]" pp_p wh
    | Letc (v, k, wh) ->
      fprintf ppf "@[<v>@[<hv>@[let %a =@] " Ident.pp v;
      fprintf ppf "@[<2>%a @]@[in @]@]" pp_cont k;
      fprintf ppf "@[%a@]@]" pp_p wh
    | Primop (pat, f, a, aa, wh) ->
      let pp_app ppf (f, aa) =
        fprintf ppf "@[<hv>%a" Ident.pp f;
        List.iteri (fun _ -> fprintf ppf " @[%a@]" maybe_pars) (a :: aa);
        fprintf ppf "@]"
      in
      fprintf ppf "@[<v>@[<hv>@[let %a =@] " pp_pat pat;
      fprintf ppf "@[<2>%a @]@[in @]@]" pp_app (f, aa);
      fprintf ppf "@[%a@]@]" pp_p wh

  and pp_binop ppf (ps, op, l, r) =
    if ps
    then fprintf ppf "(%a %a %a)" maybe_pars l Frontend.Ident.pp op maybe_pars r
    else fprintf ppf "%a %a %a" maybe_pars l Frontend.Ident.pp op maybe_pars r

  and pp_vb ppf (rec_flag, pat, p) =
    let open Args in
    let () =
      (match rec_flag with
       | Recursive -> fprintf ppf "@[<v 2>@[let rec %a "
       | NonRecursive -> fprintf ppf "@[<v 2>@[let %a ")
        pp_pat
        pat
    in
    match p with
    | Ret (HALT, Lam (pats', k, b)) ->
      let pats' = cons_uncurry @@ to_cons pats' in
      fprintf ppf "%a@ %a@ =@,@]@," pp_pats pats' Frontend.Ident.pp k;
      fprintf ppf "@[%a@]@]" pp_p b
    | Ret (HALT, t) -> fprintf ppf "=@ @]@,@[%a@]@]" no_pars t
    | _ ->
      fprintf ppf "=@,@]@,";
      fprintf ppf "@[%a@]@]" pp_p p

  and pp_pats ppf = pp_list pp_pat ppf

  and no_pars ppf = pp_triv ~ps:false ppf

  and maybe_pars ppf = pp_triv ~ps:true ppf
end

(* single-arg CPS language *)
module OneACPS = Lang (struct
    type 'a t = 'a

    let to_cons a = a, []
  end)

(* multi-arg CPS language *)
module MACPS = Lang (struct
    type 'a t = 'a * 'a list

    let to_cons (a, aa) = a, aa
  end)
