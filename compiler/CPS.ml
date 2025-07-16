(* https://www.khoury.northeastern.edu/home/shivers/papers/nobrainer-cps.pdf *)

open Frontend
open Frontend.Parsetree
open Frontend.Ident

(* ds_pattern, ds_expr, ds_vb --- that's parsetree but every var has id*)
type ds_pattern =
  | DPVar of ident
  | DPTuple of ds_pattern * ds_pattern * ds_pattern list

type ds_expr =
  | DEUnit
  | DEConst of const
  | DEVar of ident
  | DEIf of ds_expr * ds_expr * ds_expr
  | DELam of ds_pattern * ds_expr
  | DEApp of ds_expr * ds_expr
  | DETuple of ds_expr * ds_expr * ds_expr list
  | DELet of rec_flag * ds_pattern * ds_expr * ds_expr

type ds_vb = rec_flag * ds_pattern * ds_expr

let ( >>| ) = Base.Result.( >>| )
let ( let+ ) = ( >>| )
let ( let* ) = Base.Result.( >>= )

module SMap = Map.Make (String)

type 'a string_map = 'a SMap.t

module IMap = Map.Make (Int)
module ISet = Set.Make (Int)

let gensym = ANF.gensym_s

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

(* Syntax of CPS target language *)
module CPS_LANG (Args : sig
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
    | Letc of var * cont * p

  and cont =
    | Cont of pat * p
    | CVar of var
    | HALT

  and triv =
    | Lam of pat Args.t * var * p
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
    | Cont (pat, p) -> fprintf ppf "@[(fun %a ->@[ %a@])" pp_pat pat pp_p p
    | CVar v -> Frontend.Ident.pp ppf v

  and pp_triv ?(ps = true) ppf =
    let open Frontend in
    let open Args in
    function
    | Lam (pats, k, b) ->
      let pats = cons_uncurry @@ to_cons pats in
      fprintf ppf "@[(fun %a %a ->@[ %a@])" pp_pats pats Ident.pp k pp_p b
    | TSafeBinop (op, l, r) when ANF.is_infix_binop op.hum_name ->
      pp_binop ppf (ps, op, l, r)
    | UVar v -> Ident.pp ppf v
    | TConst c -> Pprint.pp_const ppf c
    | TTuple (t1, t2, tt) ->
      fprintf ppf "@[(%a, " no_pars t1;
      Format.fprintf
        ppf
        "%a"
        (pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf ", ") no_pars)
        (t2 :: tt);
      fprintf ppf ")@]"
    | TUnit -> fprintf ppf "()"
    | TSafeBinop _ -> failwith "not a binop in TSafeBinop"

  and pp_p ppf =
    let open Frontend in
    let open Args in
    function
    | Call (f, aa, k) ->
      let aa = cons_uncurry @@ to_cons aa in
      let args = pp_list maybe_pars in
      fprintf ppf "@[<hv>%a %a %a@]" maybe_pars f args aa pp_cont k
    | Ret (k, a) -> fprintf ppf "@[<hv>%a %a@]" pp_cont k maybe_pars a
    | CIf (c, th, el) ->
      fprintf ppf "@[<hov>@[if %a@ @]@[then %a@ @]@[else %a@]@]" no_pars c pp_p th pp_p el
    | Let (rec_flag, pat, Lam (pats', k, b), wh) ->
      let rec_ =
        match rec_flag with
        | Recursive -> "rec "
        | _ -> ""
      in
      let pats' = cons_uncurry @@ to_cons pats' in
      let open Ident in
      fprintf ppf "@[<v>@[<hv>@[let %s%a %a %a =@] " rec_ pp_pat pat pp_pats pats' pp k;
      fprintf ppf "@[<2>%a @]@[in @]@]" pp_p b;
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
      fprintf ppf "%a@ %a@ =@ @]@[%a@]@] " pp_pats pats' Frontend.Ident.pp k pp_p b
    | Ret (HALT, t) -> fprintf ppf "=@ @]@[%a@]@]" no_pars t
    | _ -> fprintf ppf "=@ @]@[%a@]@]" pp_p p

  and pp_pats ppf = pp_list pp_pat ppf
  and no_pars ppf = pp_triv ~ps:false ppf
  and maybe_pars ppf = pp_triv ~ps:true ppf
end

module OneACPS = CPS_LANG (struct
    type 'a t = 'a

    let to_cons a = a, []
  end)

open OneACPS

(* Abstract args *)
type a =
  | AVar of var
  | AClo of ds_pattern * ds_expr * env
  | AConst of const
  | AUnit
  | ATuple of a tuple
  | ASafeBinop of a safe_binop

and env = a IMap.t

type potent_not_allowed_expr = ds_expr option (* for rec bindings *)
type main_id = int

(* Abstract continuations *)
type c =
  | AHALT
  | KVar of var
  | FCont of ds_expr * env * c
  | ACont of a * c
  | ICont of ds_expr * ds_expr * env * c
  | TupleBldCont of ds_expr list * a list * env * c
  | LetRecCont of pat * ds_expr * env * c * potent_not_allowed_expr
  | ToplevelLetRecCont of pat * ds_vb list * main_id * potent_not_allowed_expr
  | LetNonRecCont of ds_pattern * ds_expr * env * c
  | ToplevelLetNonRecCont of ds_pattern * ds_vb list * main_id
  | BinopsFirstArgCont of var * ds_expr * env * c
  | BinopsSecondArgCont of var * a * c

type error =
  [ `Free_vars_occured of int SMap.t
  | `Let_rec_not_allowed of ds_expr
  ]

(* Extend a static environment with a new [y |-> a] entry. *)
let extend y a env = IMap.add y a env

(* Utilities to maintain reference counts of user vars in CPS term. *)
let new_count x counts = IMap.add x 0 counts
let incr x counts = IMap.add x (1 + IMap.find x counts) counts

let start_glob_envs =
  let extend v =
    let ident = Frontend.Ident.of_string v in
    SMap.add ident.hum_name ident.id
  in
  let printi = Frontend.Ident.of_string "print" in
  ( IMap.add printi.id (AVar printi) IMap.empty
  , SMap.empty
    |> extend "<"
    |> extend ">"
    |> extend "<="
    |> extend ">="
    |> extend "="
    |> extend "+"
    |> extend "-"
    |> extend "*"
    |> extend "/"
    |> SMap.add printi.hum_name printi.id )
;;

let upd id k counts no_refs ref_once =
  let n = IMap.find id counts in
  let counts = IMap.add id (n + 1) counts in
  let no_refs = if n = 0 then ISet.remove id no_refs else no_refs in
  let ref_once =
    match n with
    | 0 -> ISet.add id ref_once
    | 1 -> ISet.remove id ref_once
    | _ -> ref_once
  in
  k counts no_refs ref_once
;;

let preconv_chore ?(with_printing = false) (rec_flag, ptrn, e) k glob_vars free_vars =
  let new_count name id k vars counts no_refs =
    if with_printing then Printf.printf "var %s got id %d\n" name id;
    let vars = SMap.add name id vars in
    let no_refs = ISet.add id no_refs in
    let counts = IMap.add id 0 counts in
    k vars counts no_refs
  in
  let rec helper_p ptrn k =
    match ptrn with
    | PVar name ->
      let ident = of_string name in
      let k = k (DPVar ident) in
      new_count name ident.id k
    | PTuple (p1, p2, pp) ->
      tuple_fold_map_k helper_p p1 p2 pp (fun dp1 dp2 dps -> k (DPTuple (dp1, dp2, dps)))
  in
  let rec helper_e e vars k free_vars =
    match e with
    | EUnit -> k DEUnit free_vars
    | EConst c -> k (DEConst c) free_vars
    | EVar hum_name ->
      let builtins = snd start_glob_envs in
      let from_builtins = SMap.find_opt hum_name builtins in
      (match from_builtins with
       | Some id -> k (DEVar { hum_name; id }) free_vars
       | None ->
         let from_vars = SMap.find_opt hum_name vars in
         (match from_vars with
          | None ->
            let from_free_vars = SMap.find_opt hum_name free_vars in
            (match from_free_vars with
             | None ->
               let ident = of_string hum_name in
               let k = k (DEVar ident) in
               new_count hum_name ident.id k free_vars
             | Some id ->
               let k = k (DEVar { hum_name; id }) free_vars in
               upd id k)
          | Some id ->
            let k = k (DEVar { hum_name; id }) free_vars in
            upd id k))
    | EIf (e1, e2, e3) ->
      let k1 dp1 =
        helper_e e2 vars (fun dp2 ->
          helper_e e3 vars (fun dp3 -> k (DEIf (dp1, dp2, dp3))))
      in
      helper_e e1 vars k1 free_vars
    | ELam (ptrn, e) ->
      let k1 dp vars = helper_e e vars (fun de -> k (DELam (dp, de))) free_vars in
      helper_p ptrn k1 vars
    | EApp (e1, e2) ->
      let k1 de1 = helper_e e2 vars (fun de2 -> k (DEApp (de1, de2))) in
      helper_e e1 vars k1 free_vars
    | ETuple (e1, e2, ee) ->
      let k de1 de2 des = k (DETuple (de1, de2, des)) in
      let helper_e e k = helper_e e vars k in
      tuple_fold_map_k helper_e e1 e2 ee k free_vars
    | ELet (NonRecursive, ptrn, e1, e2) ->
      let k1 de1 free_vars =
        let k2 dp vars =
          helper_e e2 vars (fun de2 -> k (DELet (NonRecursive, dp, de1, de2))) free_vars
        in
        helper_p ptrn k2 vars
      in
      helper_e e1 vars k1 free_vars
    | ELet (Recursive, ptrn, e1, e2) ->
      let k1 dp vars =
        let k2 de1 = helper_e e2 vars (fun de2 -> k (DELet (Recursive, dp, de1, de2))) in
        helper_e e1 vars k2 free_vars
      in
      helper_p ptrn k1 vars
  in
  match rec_flag with
  | Recursive ->
    let k1 dp glob_vars =
      helper_e e glob_vars (fun de -> k (rec_flag, dp, de) glob_vars) free_vars
    in
    helper_p ptrn k1 glob_vars
  | NonRecursive ->
    let k1 de free_vars =
      let k2 dp glob_vars = k (rec_flag, dp, de) glob_vars free_vars in
      helper_p ptrn k2 glob_vars
    in
    helper_e e glob_vars k1 free_vars
;;

let test_count text =
  let vb = Frontend.Parsing.parse_vb_exn text in
  let k _ _ _ counts no_refs ref_once =
    IMap.iter (Printf.printf "id: %d; counts %d\n") counts;
    Printf.printf "ids that ref_once:\n";
    ISet.iter (Printf.printf "%d\n") ref_once;
    Printf.printf "ids that no_refs:\n";
    ISet.iter (Printf.printf "%d\n") no_refs;
    ANF.reset_gensym ()
  in
  preconv_chore
    ~with_printing:true
    vb
    k
    SMap.empty
    SMap.empty
    IMap.empty
    ISet.empty
    ISet.empty
;;

let%expect_test "counts simple" =
  test_count {| let m x y z = x y y|};
  [%expect
    {|
  var x got id 23
  var y got id 24
  var z got id 25
  var m got id 26
  id: 23; counts 1
  id: 24; counts 2
  id: 25; counts 0
  id: 26; counts 0
  ids that ref_once:
  23
  ids that no_refs:
  25
  26
|}]
;;

let%expect_test "counts branching, shadowing" =
  test_count {| let m x y = if x then fun x -> x 1 else fun x -> (y , y x)|};
  [%expect
    {|
    var x got id 27
    var y got id 28
    var x got id 29
    var x got id 30
    var m got id 31
    id: 27; counts 1
    id: 28; counts 2
    id: 29; counts 1
    id: 30; counts 1
    id: 31; counts 0
    ids that ref_once:
    27
    29
    30
    ids that no_refs:
    31
|}]
;;

let%expect_test "counts rec, ptuple" =
  test_count {| let rec (x,y) = x x y|};
  [%expect
    {|
    var x got id 32
    var y got id 33
    id: 32; counts 2
    id: 33; counts 1
    ids that ref_once:
    33
    ids that no_refs:
|}]
;;

let atuple tt =
  match List.rev tt with
  | x :: y :: tl -> ATuple (x, y, tl)
  | _ -> AUnit
;;

let rec extend_env env counts = function
  | DPVar i ->
    let x = i.hum_name |> of_string in
    CPVar x, extend i.id (AVar x) env, new_count x.id counts
  | DPTuple (p1, p2, pp) ->
    let cp1, env2, counts2 = extend_env env counts p1 in
    let cp2, env3, counts3 = extend_env env2 counts2 p2 in
    let cpp, env4, counts4 =
      List.fold_right
        (fun pn (cpp, env, counts) ->
           extend_env env counts pn |> fun (cpn, e, c) -> cpn :: cpp, e, c)
        pp
        ([], env3, counts3)
    in
    CPTuple (cp1, cp2, cpp), env4, counts4
;;

let rec has_nonzero_counts_vars counts = function
  | CPVar i -> IMap.find i.id counts <> 0
  | CPTuple (cp1, cp2, cpp) ->
    Option.is_some
    @@ Base.List.find ~f:(has_nonzero_counts_vars counts) (cp1 :: cp2 :: cpp)
;;

let maybe_not_allowed_expr e =
  match e with
  | DELam _ -> None
  | _ -> Some e
;;

(* The top-level function *)
let rec cps_glob ds_ref_once ds_no_refs glob_env =
  let open Base.Result in
  let cps_glob = cps_glob ds_ref_once ds_no_refs in
  let one_ref i = ISet.mem i.id ds_ref_once in
  let helper_toplevelletcont main_id counts = function
    | (NonRecursive, y', e) :: tl ->
      ToplevelLetNonRecCont (y', tl, main_id), e, glob_env, counts
    | [] -> AHALT, DEVar { id = main_id; hum_name = "main" }, glob_env, counts
    | (Recursive, y', e) :: tl ->
      let pat, glob_env', counts2 = extend_env glob_env counts y' in
      ( ToplevelLetRecCont (pat, tl, main_id, maybe_not_allowed_expr e)
      , e
      , glob_env'
      , counts2 )
  in
  let rec cps env exp c counts =
    match exp with
    | DEVar y -> ret c (IMap.find y.id env) counts
    | DEConst z -> ret c (AConst z) counts
    | DEUnit -> ret c AUnit counts
    | DELam (ds_pat, e) -> ret c (AClo (ds_pat, e, env)) counts
    | DEApp (DEApp (DEVar op, e1), e2) when ANF.is_infix_binop op.hum_name ->
      cps env e1 (BinopsFirstArgCont (op, e2, env, c)) counts
    | DEApp (e1, e2) -> cps env e1 (FCont (e2, env, c)) counts
    | DEIf (e1, e2, e3) -> cps env e1 (ICont (e2, e3, env, c)) counts
    | DELet (NonRecursive, ds_pat, e1, e2) ->
      cps env e1 (LetNonRecCont (ds_pat, e2, env, c)) counts
    | DELet (Recursive, ds_pat, e1, e2) ->
      let pat, env', counts' = extend_env env counts ds_pat in
      cps env' e1 (LetRecCont (pat, e2, env', c, maybe_not_allowed_expr e1)) counts'
    | DETuple (e1, e2, ee) -> cps env e1 (TupleBldCont (e2 :: ee, [], env, c)) counts
  (* Three smart constructors, for RET, CALL & IF forms. *)
  and ret c a counts =
    match c with
    | AHALT | KVar _ ->
      let* cont, counts2 = blessc c counts in
      let+ arg, counts3 = blessa a counts2 in
      Ret (cont, arg), counts3
    | FCont (e, env, c') -> cps env e (ACont (a, c')) counts
    | ACont (a', c') -> call a' a c' counts
    | ICont (e1, e2, env, c') -> cif a e1 e2 c' env counts
    | TupleBldCont (ds_ee, aa, env, c') ->
      (match ds_ee with
       | [] -> ret c' (atuple (a :: aa)) counts
       | hd :: tl -> cps env hd (TupleBldCont (tl, a :: aa, env, c')) counts)
    | LetNonRecCont (y, wh, env, c') ->
      bnd cps y a wh env c' (fun x b w -> Let (NonRecursive, x, b, w)) counts
    | LetRecCont (pat, wh, env, c', dang_expr) ->
      let constr b w = Let (Recursive, pat, b, w) in
      bnd_rec cps a wh env c' constr counts dang_expr pat
    | ToplevelLetNonRecCont (y, vbs, main_id) ->
      let constr x b w = Let (NonRecursive, x, b, w) in
      let c'', e, glob_env', counts2 = helper_toplevelletcont main_id counts vbs in
      bnd cps_glob y a e glob_env' c'' constr counts2
    | ToplevelLetRecCont (pat, vbs, main_id, dang_expr) ->
      let c'', e, glob_env', counts2 = helper_toplevelletcont main_id counts vbs in
      let constr b w = Let (Recursive, pat, b, w) in
      bnd_rec cps_glob a e glob_env' c'' constr counts2 dang_expr pat
    | BinopsFirstArgCont (op, e, env, c') ->
      cps env e (BinopsSecondArgCont (op, a, c')) counts
    | BinopsSecondArgCont (op, a1, c') -> binop op a1 a c' counts
  and call f a c counts =
    match f with
    | AVar v when String.equal v.hum_name "print" -> primop v a [] c counts
    | AVar _ | AConst _ | AUnit | ASafeBinop _ | ATuple _ ->
      let* func, arg, counts2 = blessa2 f a counts in
      let+ cont, counts3 = blessc c counts2 in
      Call (func, arg, cont), counts3
    | AClo (y, body, env) ->
      bnd cps y a body env c (fun x arg b -> Ret (Cont (x, b), arg)) counts
  and cif a e1 e2 c env counts =
    match c with
    (* unit size conts*)
    | AHALT | KVar _ ->
      let* test, counts2 = blessa a counts in
      let* conseq, counts3 = cps env e1 c counts2 in
      let+ alt, counts4 = cps env e2 c counts3 in
      CIf (test, conseq, alt), counts4
    (* other conts*)
    | _ ->
      let jv = gensym ~prefix:"jv" () |> of_string in
      let* body, counts2 = cif a e1 e2 (KVar jv) env counts in
      let+ join, counts3 = blessc c counts2 in
      Letc (jv, join, body), counts3
  and bnd cps_func y a wh env c constr counts =
    match y with
    | DPTuple _ ->
      let* b, counts2 = blessa a counts in
      let pat, env', counts3 = extend_env env counts2 y in
      let+ w, counts4 = cps_func env' wh c counts3 in
      constr pat b w, counts4
    | DPVar i ->
      if one_ref i
      then cps_func (extend i.id a env) wh c counts
      else
        let* b, counts2 = blessa a counts in
        (match b, ISet.mem i.id ds_no_refs with
         | TUnit, false -> cps_func (extend i.id AUnit env) wh c counts2
         | UVar x, false -> cps_func (extend i.id (AVar x) env) wh c counts2
         | TConst z, false -> cps_func (extend i.id (AConst z) env) wh c counts2
         | _ ->
           let pat, env', counts3 = extend_env env counts2 y in
           let+ w, counts4 = cps_func env' wh c counts3 in
           constr pat b w, counts4)
  and bnd_rec cps_func a wh env c constr counts dang_expr pat =
    let* b, counts2 = blessa a counts in
    match dang_expr with
    | Some e when has_nonzero_counts_vars counts2 pat -> Error (`Let_rec_not_allowed e)
    | _ ->
      let+ w, counts3 = cps_func env wh c counts2 in
      constr b w, counts3
  and binop op a1 a2 c counts =
    match a1, a2, op.hum_name with
    | _, (AConst (PConst_int 0) | AVar _), "/" -> primop op a1 [ a2 ] c counts
    | _ -> ret c (ASafeBinop (op, a1, a2)) counts
  and primop f a aa c counts =
    let* counts2, args = blessa_many counts aa in
    let* arg, counts3 = blessa a counts2 in
    match c with
    | LetNonRecCont ((DPVar _ as dp_pat), wh, env, c') ->
      let cp_pat, env', counts4 = extend_env env counts3 dp_pat in
      let+ w, counts5 = cps env' wh c' counts4 in
      Primop (cp_pat, f, arg, args, w), counts5
    | ToplevelLetNonRecCont ((DPVar _ as dp_pat), vbs, main_id) ->
      let c', e, glob_env', counts4 = helper_toplevelletcont main_id counts3 vbs in
      let cp_pat, glob_env'', counts5 = extend_env glob_env' counts4 dp_pat in
      let+ wh, counts6 = cps_glob glob_env'' e c' counts5 in
      Primop (cp_pat, f, arg, args, wh), counts6
    | _ ->
      let x = gensym ~prefix:"x" () |> of_string in
      let counts4 = new_count x.id counts3 in
      let+ wh, counts5 = ret c (AVar x) counts4 in
      Primop (CPVar x, f, arg, args, wh), counts5
  (* Two "blessing" functions to render abstract continuations
     and abstract arguments into actual syntax. *)
  and blessc c counts =
    match c with
    | AHALT -> Ok (HALT, counts)
    | KVar kv -> Ok (CVar kv, counts)
    | _ ->
      let x = gensym ~prefix:"t" () |> of_string in
      let counts2 = new_count x.id counts in
      let+ body, counts3 = ret c (AVar x) counts2 in
      Cont (CPVar x, body), counts3
  and blessa a counts =
    match a with
    | AUnit -> Ok (TUnit, counts)
    | AVar x -> Ok (UVar x, incr x.id counts)
    | AConst z -> Ok (TConst z, counts)
    | ATuple (a1, a2, aa) ->
      let* t1, t2, counts2 = blessa2 a1 a2 counts in
      let+ counts3, tt = blessa_many counts2 aa in
      TTuple (t1, t2, tt), counts3
    | ASafeBinop (op, a1, a2) ->
      let+ arg1, arg2, counts2 = blessa2 a1 a2 counts in
      TSafeBinop (op, arg1, arg2), counts2
    | AClo (y, body, env) ->
      let pat, env', counts2 = extend_env env counts y in
      let k = gensym ~prefix:"k" () |> of_string in
      let+ b, counts3 = cps env' body (KVar k) counts2 in
      (* The eta-reduction check. Note that we don't have to check
         reference counts on k, as continuation variables are linear. *)
      (match b, pat with
       | Call (f, UVar x', CVar k'), CPVar x ->
         if x = x' && k = k' && IMap.find x.id counts3 = 1
         then f, counts3
         else Lam (pat, k, b), counts3
       | _ -> Lam (pat, k, b), counts3)
  and blessa_many counts aa =
    let+ counts2, rev_tt =
      Base.List.fold_result
        ~f:(fun (counts, tt) a -> blessa a counts >>| fun (t, counts) -> counts, t :: tt)
        ~init:(counts, [])
        aa
    in
    counts2, List.rev rev_tt
  and blessa2 a1 a2 counts =
    let* triv1, counts2 = blessa a1 counts in
    let+ triv2, counts3 = blessa a2 counts2 in
    triv1, triv2, counts3
  in
  cps glob_env
;;

let free_vars_check k free_vars =
  let has_not_free_vars = SMap.is_empty free_vars in
  if has_not_free_vars then k () else Error (`Free_vars_occured free_vars)
;;

let cps_conv_vb vb =
  let open IMap in
  let k ds_vb _ free_vars _ no_refs ref_once =
    let k1 () = Ok (ds_vb, no_refs, ref_once) in
    free_vars_check k1 free_vars
  in
  let* (rec_flag, ds_pat, ds_expr), ds_no_refs, ds_ref_once =
    preconv_chore vb k (snd start_glob_envs) SMap.empty IMap.empty ISet.empty ISet.empty
  in
  let pat, glob_env, counts = extend_env (fst start_glob_envs) empty ds_pat in
  let+ p, _ =
    match rec_flag with
    | Recursive -> cps_glob ds_ref_once ds_no_refs glob_env ds_expr AHALT counts
    | NonRecursive ->
      cps_glob ds_ref_once ds_no_refs (fst start_glob_envs) ds_expr AHALT empty
  in
  rec_flag, pat, p
;;

let cps_conv_program vbs =
  let open IMap in
  let open Ident in
  let ( let+ ) = Base.Result.( >>| ) in
  let k ds_vbs glob_vars free_vars counts no_refs ref_once =
    let main_id = SMap.find "main" glob_vars in
    let k2 _ no_refs ref_once = Ok (ds_vbs, no_refs, ref_once, main_id) in
    let k1 () = upd main_id k2 counts no_refs ref_once in
    free_vars_check k1 free_vars
  in
  let* ds_vbs, ds_no_refs, ds_ref_once, main_id =
    list_fold_map_k
      (preconv_chore ~with_printing:false)
      vbs
      k
      (snd start_glob_envs)
      SMap.empty
      IMap.empty
      ISet.empty
      ISet.empty
  in
  let cps_glob = cps_glob ds_ref_once ds_no_refs in
  let+ p, _ =
    match ds_vbs with
    | [] -> Ok (Ret (HALT, TUnit), empty)
    | (NonRecursive, ds_pat, ds_expr) :: tl ->
      cps_glob
        (fst start_glob_envs)
        ds_expr
        (ToplevelLetNonRecCont (ds_pat, tl, main_id))
        empty
    | (Recursive, ds_pat, ds_expr) :: tl ->
      let pat, glob_env, counts = extend_env (fst start_glob_envs) empty ds_pat in
      cps_glob
        glob_env
        ds_expr
        (ToplevelLetRecCont (pat, tl, main_id, maybe_not_allowed_expr ds_expr))
        counts
  in
  NonRecursive, CPVar (of_string "main"), p
;;

let ds_expr_to_expr ds_expr =
  let rec helper_p dp k =
    match dp with
    | DPVar { hum_name = n; _ } -> k (PVar n)
    | DPTuple (dp1, dp2, dpp) ->
      tuple_fold_map_k helper_p dp1 dp2 dpp (fun ptrn1 ptrn2 pp ->
        k (PTuple (ptrn1, ptrn2, pp)))
  in
  let rec helper de k =
    match de with
    | DEUnit -> k EUnit
    | DEConst c -> k (EConst c)
    | DEVar { hum_name = n; _ } -> k (EVar n)
    | DEIf (de1, de2, de3) ->
      helper de1 (fun e1 ->
        helper de2 (fun e2 -> helper de3 (fun e3 -> k (EIf (e1, e2, e3)))))
    | DELam (dp, de) -> helper_p dp (fun ptrn -> helper de (fun e -> k (ELam (ptrn, e))))
    | DEApp (de1, de2) -> helper de1 (fun e1 -> helper de2 (fun e2 -> k (EApp (e1, e2))))
    | DETuple (de1, de2, dee) ->
      tuple_fold_map_k helper de1 de2 dee (fun e1 e2 ee -> k (ETuple (e1, e2, ee)))
    | DELet (rec_flag, dp, de1, de2) ->
      helper_p dp (fun ptrn ->
        helper de1 (fun e1 -> helper de2 (fun e2 -> k (ELet (rec_flag, ptrn, e1, e2)))))
  in
  helper ds_expr Fun.id
;;

let pp_error ppf : error -> _ = function
  | `Let_rec_not_allowed ds_expr ->
    Format.fprintf
      ppf
      " %a: This kind of expression is not allowed as right-hand side of `let rec'"
      Pprint.pp_expr
      (ds_expr_to_expr ds_expr)
  | `Free_vars_occured vars ->
    let msg =
      String.concat
        "\n"
        ("Variables are not in scope:" :: SMap.fold (fun v _ acc -> v :: acc) vars [])
    in
    Format.fprintf ppf "%s" msg
;;

let test_cps_program text =
  let open Frontend in
  let stru = Result.get_ok @@ Parsing.parse_structure text in
  match cps_conv_program stru with
  | Ok cps_prog ->
    Format.printf "%a" pp_vb cps_prog;
    ANF.reset_gensym ()
  | Error e -> Format.printf "%a\n%!" pp_error e
;;

let test_cps_vb text =
  let open Frontend in
  match cps_conv_vb @@ Parsing.parse_vb_exn text with
  | Ok cps_vb ->
    Format.printf "%a" pp_vb cps_vb;
    ANF.reset_gensym ()
  | Error e -> Format.printf "%a\n%!" pp_error e
;;

let%expect_test "cps simple func" =
  test_cps_vb {| let double x = 2 * x|};
  [%expect
    {| let double x k1 = k1 (2 * x)
|}]
;;

let%expect_test "cps simple prog" =
  test_cps_program
    {| let double x = 2 * x
  let main = double (double 3)|};
  [%expect
    {|
let main = let double x k1 = k1 (2 * x) in double 3 (fun t2 -> double t2
                                                               (fun t3 ->
                                                                (fun x -> x) t3))
|}]
;;

let%expect_test "cps prog inlining" =
  test_cps_program
    {|let y = 3
  let double x = 2 * x
  let main = double (y + y)|};
  [%expect
    {|  let main = 2 * (3 + 3)
|}]
;;

let%expect_test "cps rec func (inlining banned)" =
  test_cps_vb {| let y = let rec t x = t 1 in t 2|};
  [%expect
    {| let y = let rec t x k1 = t 1 k1 in t 2 (fun x -> x)
|}]
;;

let%expect_test "cps eta" =
  test_cps_vb
    {|
   let main = let g x = x in (fun x -> g x) g|};
  [%expect
    {| let main = let g x k1 = k1 x in g g (fun x -> x)
|}]
;;

let%expect_test "cps eta let" =
  test_cps_vb
    {|
   let main = let g x = x in let f y = g y in f (g 0)|};
  [%expect
    {| let main = let g x k1 = k1 x in g 0 (fun t2 -> g t2 (fun x -> x))
|}]
;;

let%expect_test "cps fac" =
  test_cps_vb {| let rec fac n = if n = 1 then 1 else fac (n-1) * n|};
  [%expect
    {| let rec fac n k1 = if n = 1 then k1 1 else fac (n - 1) (fun t2 -> k1 (t2 * n))
|}]
;;

let%expect_test "cps fib" =
  test_cps_vb
    {|
  let rec  fib n =
if n < 2 then n else fib (n - 1) + fib (n - 2)
  |};
  [%expect
    {|
let rec fib n k1 = if n < 2 then k1 n else fib (n - 1) (fun t2 -> fib (n - 2)
                                                                  (fun t3 ->
                                                                   k1 (t2 + t3)))|}]
;;

let%expect_test "cps complex branching" =
  test_cps_vb {| let x f = 1 + if (f 2) then 3 else 5|};
  [%expect
    {|
    let x f k1 = f 2 (fun t2 -> let jv3 t4 = k1 (1 + t4) in if t2 then jv3 3
                                                            else jv3 5)
|}]
;;

let%expect_test "cps print" =
  test_cps_vb {|let main  = (fun z -> 1) (print 0) |};
  [%expect
    {|   let main = let x1 = print 0 in (fun z -> (fun x -> x) 1) x1
|}]
;;

let%expect_test "cps print alias " =
  test_cps_vb {| let f = let p = print in let z = p 0 in z + 1|};
  [%expect
    {| let f = let z = print 0 in (fun x -> x) (z + 1)
|}]
;;

let%expect_test "cps one ref arg-binop" =
  test_cps_vb {| let f = let g x = x + 1 in g (2 * 2)   |};
  [%expect
    {| let f = (2 * 2) + 1
|}]
;;

let%expect_test "cps one ref binop" =
  test_cps_vb {| let f g = let x = 2 * 2 in g x |};
  [%expect
    {| let f g k1 = g (2 * 2) k1
|}]
;;

let%expect_test "cps mult refs arg-const" =
  test_cps_vb {| let f g = let x = 2 in g x x|};
  [%expect
    {| let f g k1 = g 2 (fun t2 -> t2 2 k1)
|}]
;;

let%expect_test "cps  mult refs arg-binop (inlining banned)" =
  test_cps_vb {| let f g = let x = 2 * 2 in g x x|};
  [%expect
    {| let f g k1 = let x = 2 * 2 in g x (fun t2 -> t2 x k1)
|}]
;;

let%expect_test "cps complex tuple-arg" =
  test_cps_vb {| let f g = g (g 3, 1)|};
  [%expect
    {| let f g k1 = g 3 (fun t2 -> g (t2, 1) k1)
|}]
;;

let%expect_test "cps ptuple" =
  test_cps_vb {| let f (x,y) = x + y|};
  [%expect
    {| let f (x, y) k1 = k1 (x + y)
|}]
;;

let%expect_test "cps free vars" =
  test_cps_vb {| let f x = x + y + z|};
  [%expect
    {|
  Variables are not in scope:
  z
  y
|}]
;;

let%expect_test "cps func in func" =
  test_cps_vb {| let z = let rec g y = y in  let f = fun x -> g in f 2 3|};
  [%expect
    {|  let z = let rec g y k1 = k1 y in (fun x -> g 3 (fun x -> x)) 2
|}]
;;

let%expect_test "cps not allowed let rec" =
  test_cps_vb {| let main  = let rec  x =  x 0 in 0|};
  [%expect
    {|  (x 0): This kind of expression is not allowed as right-hand side of `let rec'
|}]
;;

let%expect_test "cps not allowed let rec lambda complex" =
  test_cps_vb {| let main  = let rec x = (fun z -> (fun y -> x )) 0 in 0|};
  [%expect
    {|  ((fun z -> (fun y -> x)) 0): This kind of expression is not allowed as right-hand side of `let rec'
|}]
;;

let%expect_test "cps fake rec" =
  test_cps_vb {| let main  = let rec x = (fun y -> 8) 12 in 0|};
  [%expect
    {|  let main = (fun y -> let rec x = 8 in (fun x -> x) 0) 12
|}]
;;

(* multi-arg CPS language *)
module MACPS = CPS_LANG (struct
    type 'a t = 'a * 'a list

    let to_cons (a, aa) = a, aa
  end)

module CallArityAnal (CoCallGraph : sig
    type t

    val empty : t
    val adj_nodes : int -> t -> unit IMap.t
    val has_loop : int -> t -> bool
    val cartesian : unit IMap.t -> unit IMap.t -> t
    val cartesian_square : unit IMap.t -> t
    val union : t -> t -> t
    val remove : int -> t -> t
  end) : sig
  val call_arity_anal : cps_vb -> MACPS.cps_vb
  val call_arity_anal_debug : cps_vb -> MACPS.cps_vb
end = struct
  type ress =
    { dead_vars : ISet.t
    ; call_ars : int IMap.t
    }

  open CoCallGraph

  let find_with_default key m default =
    match IMap.find key m with
    | el -> el
    | exception Not_found -> default
  ;;

  let anal (_, _, p) =
    let ( @@@ ) = IMap.union (fun _ () () -> Some ()) in
    let ar_union = IMap.union (fun _ x y -> Some (min x y)) in
    let domain = IMap.map ignore in
    let leave_vars_scope co_calls ars v_id = remove v_id co_calls, IMap.remove v_id ars in
    let cond_add v_id v_arity cond ress =
      if cond v_arity
      then { ress with call_ars = IMap.add v_id v_arity ress.call_ars }
      else ress
    in
    let add_if_incr v_arity t =
      match t with
      | Lam (_, { id = res_id; _ }, _) -> cond_add res_id v_arity @@ ( < ) 1
      | _ -> Fun.id
    in
    let add_if_pos v_id v_arity = cond_add v_id v_arity @@ ( < ) 0 in
    let is_int_triv_rhs int = function
      | Lam _ -> true
      | UVar { id; _ } -> ISet.mem id int
      | TSafeBinop _ | TConst _ | TTuple _ | TUnit -> false
    in
    let ignore_fst (_, b, c) = b, c in
    let ret_with_fv ((_, ars, _) as fst) = fst, domain ars in
    let ret_with_fv_bad (_, ars, ress) =
      let rhs_fv = domain ars in
      (cartesian_square rhs_fv, ars, ress), rhs_fv
    in
    let ret_with_fv_cond ?(loop_info = None) v_id v_arity b_co_calls =
      let has_loop =
        match loop_info with
        | Some i -> i
        | None -> has_loop v_id b_co_calls
      in
      if (not has_loop) || v_arity = 0 then ret_with_fv else ret_with_fv_bad
    in
    let rec anal_p conts ress inc_ar p safe_ars int =
      let open IMap in
      let calc_safe_ar_t =
        let rec helper acc = function
          | UVar { id; _ } -> acc + find_with_default id safe_ars 0
          | Lam (_, _, b) ->
            let acc2 = acc + 1 in
            let rec lam_b_helper = function
              | Ret ((HALT | CVar _), t) -> helper acc2 t
              | CIf (_, th, el) -> min (lam_b_helper th) @@ lam_b_helper el
              | _ -> acc2
            in
            lam_b_helper b
          | _ -> acc
        in
        helper 0
      in
      let upd_safe_ars_if_ge_2 id t =
        match calc_safe_ar_t t with
        | n when n >= 2 -> add id n safe_ars
        | _ -> safe_ars
      in
      let call_bnd_safe_ars_hndl ?(ignore_safe_ars = false) id f =
        match calc_safe_ar_t f with
        | n when n >= 3 && not ignore_safe_ars -> `Safe, add id n safe_ars
        | n when n >= 2 -> `Safe, safe_ars
        | _ -> `UnSafe, safe_ars
      in
      let anal_tt0_ign a aa ress2 =
        anal_tt0 conts safe_ars int a aa ress2 |> ignore_fst
      in
      let anal_triv0_ign t ress = anal_triv conts safe_ars int 0 t ress |> ignore_fst in
      let lam_call_anal0_ign lam_b p a ress =
        lam_call_anal lam_b conts safe_ars int 0 ress p a |> ignore_fst
      in
      let clear_lam_call_anal lam_b =
        lam_call_anal lam_b conts safe_ars int inc_ar ress
      in
      let clear_triv_ret_anal t = anal_triv conts safe_ars int inc_ar t ress in
      match p with
      | CIf (c, th, el) -> anal_cif c th el conts safe_ars int ress inc_ar
      | Call ((Lam (pat, c, lam_b) as f), a, Cont (CPVar { id; _ }, body)) ->
        let had_upd, (is_safe, safe_ars2) = false, call_bnd_safe_ars_hndl id f in
        fin_lam_call_bnd_anal ~had_upd c lam_b conts safe_ars2 int pat a is_safe
        @@ anal_bnd1 id conts body int ress inc_ar safe_ars2
      | Call (Lam (pat, _, lam_b), a, Cont (CPTuple _, body)) ->
        let rhs_anal = lam_call_anal0_ign lam_b pat a in
        fin_unint_bnd_anal rhs_anal @@ anal_p conts ress inc_ar body safe_ars int
      | Call ((Lam (pat, c, lam_b) as f), a, CVar { id; _ }) ->
        (match find id conts with
         | `Int fin_anal ->
           let had_upd = true in
           let is_safe, _ = call_bnd_safe_ars_hndl ~ignore_safe_ars:true id f in
           (ress, fin_anal)
           |> fin_lam_call_bnd_anal ~had_upd c lam_b conts safe_ars int pat a is_safe
         | `UnInt (b_co_calls, b_ars) ->
           fin_unint_bnd_anal (lam_call_anal0_ign lam_b pat a) (b_co_calls, b_ars, ress)
         | exception Not_found -> clear_lam_call_anal lam_b pat a)
      | Call (Lam (pat, _, lam_b), a, HALT) -> clear_lam_call_anal lam_b pat a
      | Call (f, a, Cont (CPVar { id; _ }, body)) ->
        let is_safe, safe_ars2 = call_bnd_safe_ars_hndl id f in
        fin_bnd_call_anal is_safe conts safe_ars int f a
        @@ anal_bnd1 id conts body int ress inc_ar safe_ars2
      | Call (f, a, CVar { id; _ }) ->
        (match find id conts with
         | `Int fin_anal ->
           let is_safe, _ = call_bnd_safe_ars_hndl ~ignore_safe_ars:true id f in
           fin_bnd_call_anal is_safe conts safe_ars int f a (ress, fin_anal)
         | `UnInt (b_co_calls, b_ars) ->
           (b_co_calls, b_ars, ress) |> fin_unint_bnd_anal @@ anal_tt0_ign f [ a ]
         | exception Not_found ->
           anal_tt0 ~fst_inc_ar:(inc_ar + 1) conts safe_ars int f [ a ] ress)
      | Call (f, a, HALT) ->
        anal_tt0 ~fst_inc_ar:(inc_ar + 1) conts safe_ars int f [ a ] ress
      | Ret (CVar { id; _ }, t) ->
        (match find id conts with
         | `Int fin_anal ->
           fin_int_triv_bnd_anal ~had_upd:true conts safe_ars int t (ress, fin_anal)
         | `UnInt (b_co_calls, b_ars) ->
           fin_unint_bnd_anal (anal_triv0_ign t) (b_co_calls, b_ars, ress)
         | exception Not_found -> clear_triv_ret_anal t)
      | Ret (Cont (CPVar { id; _ }, body), t)
      | Let (NonRecursive, CPVar { id; _ }, t, body)
        when is_int_triv_rhs int t ->
        fin_int_triv_bnd_anal conts safe_ars int t
        @@ anal_bnd1 id conts body int ress inc_ar
        @@ upd_safe_ars_if_ge_2 id t
      | Ret (Cont ((CPTuple _ | CPVar _), body), t)
      | Let (NonRecursive, CPVar _, t, body)
      | Let (_, CPTuple _, t, body) ->
        fin_unint_bnd_anal (anal_triv0_ign t)
        @@ anal_p conts ress inc_ar body safe_ars int
      | Primop (_, _, t, tt, body) ->
        fin_unint_bnd_anal (anal_tt0_ign t tt)
        @@ anal_p conts ress inc_ar body safe_ars int
      | Call (f, a, Cont (CPTuple _, body)) ->
        fin_unint_bnd_anal (anal_tt0_ign f [ a ])
        @@ anal_p conts ress inc_ar body safe_ars int
      | Letc ({ id = jv_id; _ }, Cont (CPVar { id; _ }, body), p) ->
        let jv_specif = Some jv_id in
        let ress2, fin_anal =
          anal_bnd1 ~jv_specif id conts body int ress inc_ar safe_ars
        in
        let conts2 = add jv_id (`Int fin_anal) conts in
        anal_p conts2 ress2 0 p safe_ars int
      | Letc ({ id = jp_id; _ }, Cont (CPTuple _, body), p) ->
        let b_co_calls, b_ars, ress2 = anal_p conts ress inc_ar body safe_ars int in
        let conts2 = add jp_id (`UnInt (b_co_calls, b_ars)) conts in
        anal_p conts2 ress2 0 p safe_ars int
      | Letc ({ id = jp_id1; _ }, CVar { id = jp_id2; _ }, p) ->
        let upd_ress () =
          if ISet.mem jp_id2 ress.dead_vars
          then { ress with dead_vars = ISet.add jp_id1 ress.dead_vars }
          else (
            match find jp_id2 ress.call_ars with
            | n -> { ress with call_ars = add jp_id1 n ress.call_ars }
            | exception Not_found -> ress)
        in
        let conts2, ress2 =
          match find jp_id2 conts with
          | `Int _ as c -> add jp_id1 c conts, upd_ress ()
          | `UnInt _ as c -> add jp_id1 c conts, ress
          | exception Not_found -> conts, ress
        in
        anal_p conts2 ress2 inc_ar p safe_ars int
      | Ret (HALT, t) -> clear_triv_ret_anal t
      | Letc (_, HALT, p) -> anal_p conts ress inc_ar p safe_ars int
      | Let (Recursive, CPVar { id; _ }, t, body) ->
        let safe_ars2 = upd_safe_ars_if_ge_2 id t in
        let int2 = ISet.add id int in
        anal_rec_bnd id conts safe_ars2 int2 t
        @@ anal_p conts ress inc_ar body safe_ars2 int2
    and anal_bnd1 ?(jv_specif = None) id conts body int ress inc_ar safe_ars =
      anal_bnd_cont ~jv_specif id
      @@ anal_p conts ress inc_ar body safe_ars
      @@ ISet.add id int
    and anal_bnd_cont ?(jv_specif = None) v_id (b_co_calls, b_ars, ress) =
      match IMap.find v_id b_ars with
      | exception Not_found ->
        (* dead var case *)
        let dead_id = Option.value jv_specif ~default:v_id in
        ( { ress with dead_vars = ISet.add dead_id ress.dead_vars }
        , fun ress2 _ -> b_co_calls, b_ars, ress2 )
      | v_arity ->
        let k_co_calls, k_ars = leave_vars_scope b_co_calls b_ars v_id in
        let neigh = adj_nodes v_id b_co_calls |> IMap.remove v_id in
        let ress2 =
          match jv_specif with
          | None -> ress
          | Some res_id -> add_if_pos res_id v_arity ress
        in
        ( ress2
        , fun _ non_dead_hndl ->
            let (rhs_co_calls, rhs_ars, ress3), rhs_fv =
              non_dead_hndl b_co_calls v_id v_arity
            in
            let p_ars = ar_union k_ars rhs_ars in
            let p_co_calls =
              union k_co_calls @@ union rhs_co_calls @@ cartesian rhs_fv neigh
            in
            p_co_calls, p_ars, ress3 )
    and anal_rec_bnd v_id conts safe_ars int t (b_co_calls, b_ars, ress) =
      match IMap.find v_id b_ars with
      | exception Not_found ->
        (* dead var case *)
        b_co_calls, b_ars, { ress with dead_vars = ISet.add v_id ress.dead_vars }
      | v_arity ->
        let rec fixpointing v_ar has_loop_v =
          let (rhs_co_calls, rhs_ars, ress2), rhs_fv =
            anal_triv conts safe_ars int v_ar t ress
            |> ret_with_fv_cond ~loop_info:(Some has_loop_v) v_id v_ar b_co_calls
          in
          let p_co_calls =
            union b_co_calls
            @@ union rhs_co_calls
            @@ cartesian rhs_fv
            @@ adj_nodes v_id
            @@ union b_co_calls rhs_co_calls
          in
          let new_v_ar =
            match IMap.find v_id rhs_ars with
            | n when n <= v_ar -> n
            | (exception Not_found) | _ -> v_ar
          in
          let new_has_loop_v = has_loop v_id p_co_calls in
          if new_v_ar <> v_ar || new_has_loop_v <> has_loop_v
          then fixpointing new_v_ar new_has_loop_v
          else p_co_calls, ar_union b_ars rhs_ars, add_if_incr v_ar t ress2
        in
        fixpointing v_arity @@ has_loop v_id b_co_calls
    and fin_bnd_call_anal is_safe conts safe_ars int f a (ress, fin_anal) =
      fin_anal ress
      @@ fun b_co_calls v_id v_arity ->
      let fst_inc_ar =
        1 + if has_loop v_id b_co_calls && is_safe = `UnSafe then 0 else v_arity
      in
      anal_tt0 ~fst_inc_ar conts safe_ars int f [ a ] ress |> ret_with_fv
    and fin_int_triv_bnd_anal ?(had_upd = false) conts safe_ars int t (ress, fin_anal) =
      fin_anal ress
      @@ fun b_co_calls v_id v_arity ->
      let ress2 = if had_upd then ress else add_if_incr v_arity t ress in
      anal_triv conts safe_ars int v_arity t ress2
      |> ret_with_fv_cond v_id v_arity b_co_calls
    and fin_unint_bnd_anal anal_rhs (b_co_calls, b_ars, ress) =
      let b_fv = domain b_ars in
      let rhs_ars, ress2 = anal_rhs ress in
      let rhs_fv = domain rhs_ars in
      let p_ars = ar_union b_ars rhs_ars in
      let p_co_calls =
        union b_co_calls @@ union (cartesian_square rhs_fv) @@ cartesian rhs_fv b_fv
      in
      p_co_calls, p_ars, ress2
    and lam_call_anal lam_b conts safe_ars int inc_ar ress pat t =
      match pat, t with
      | CPVar { id; _ }, t when is_int_triv_rhs int t ->
        fin_int_triv_bnd_anal conts safe_ars int t
        @@ anal_bnd_cont id
        @@ anal_p conts ress inc_ar lam_b safe_ars
        @@ ISet.add id int
      | _, t ->
        fin_unint_bnd_anal (fun ress' ->
          anal_triv conts safe_ars int inc_ar t ress' |> ignore_fst)
        @@ anal_p conts ress inc_ar lam_b safe_ars int
    and fin_lam_call_bnd_anal ~had_upd c lam_b conts safe_ars int pat t is_safe rf =
      let ress, fin_anal = rf in
      fin_anal ress
      @@ fun b_co_calls v_id v_arity ->
      let inc_ar = if has_loop v_id b_co_calls && is_safe = `UnSafe then 0 else v_arity in
      let ress2 = if had_upd then ress else add_if_pos c.id inc_ar ress in
      lam_call_anal lam_b conts safe_ars int inc_ar ress2 pat t
      |> ret_with_fv_cond v_id v_arity b_co_calls
    and anal_cif c th el conts safe_ars int ress inc_ar =
      let c_co_calls, c_ars, ress2 = anal_triv conts safe_ars int 0 c ress in
      let th_co_calls, th_ars, ress3 = anal_p conts ress2 inc_ar th safe_ars int in
      let el_co_calls, el_ars, ress4 = anal_p conts ress3 inc_ar el safe_ars int in
      let p_ars = ar_union c_ars @@ ar_union th_ars el_ars in
      let p_co_calls =
        union c_co_calls
        @@ union th_co_calls
        @@ union el_co_calls
        @@ cartesian (domain c_ars)
        @@ domain th_ars
        @@@ domain el_ars
      in
      p_co_calls, p_ars, ress4
    and anal_triv conts safe_ars int inc_ar t ress =
      match t, inc_ar with
      | UVar { id; _ }, _ when ISet.mem id int -> empty, IMap.singleton id inc_ar, ress
      | (TUnit | TConst _ | UVar _), _ -> empty, IMap.empty, ress
      | TTuple (t1, t2, tt), _ -> anal_tt0 conts safe_ars int t1 (t2 :: tt) ress
      | TSafeBinop (_, t1, t2), _ -> anal_tt0 conts safe_ars int t1 [ t2 ] ress
      | Lam (_, _, lam_b), 0 ->
        let _, b_ars, ress2 = anal_p conts ress 0 lam_b safe_ars int in
        cartesian_square @@ domain b_ars, b_ars, ress2
      | Lam (_, _, lam_b), _ ->
        let inc_ar2 = Int.max 0 (inc_ar - 1) in
        anal_p conts ress inc_ar2 lam_b safe_ars int
    and anal_tt0 ?(fst_inc_ar = 0) conts safe_ars int t1 tt ress =
      let anal_triv_sh = anal_triv conts safe_ars int 0 in
      let f ((cc, ars, ress), (fv1, fv_acc)) t =
        let cc_t, ars_t, ress2 = anal_triv_sh t ress in
        let fv_acc2 = fv1 @@@ fv_acc in
        let fv_t = domain ars_t in
        let cc2 = union cc @@ union cc_t @@ cartesian fv_acc2 fv_t in
        (cc2, ar_union ars ars_t, ress2), (fv_t, fv_acc)
      in
      let init =
        let ((_, ars_t, _) as frst) = anal_triv conts safe_ars int fst_inc_ar t1 ress in
        frst, (IMap.empty, domain ars_t)
      in
      List.fold_left f init tt |> fst
    in
    let open IMap in
    anal_p empty { dead_vars = ISet.empty; call_ars = empty } 0 p empty ISet.empty
    |> fun (_, _, ress) -> ress.dead_vars, ress.call_ars
  ;;

  let foldd_k f =
    let rec helper lst k =
      match lst with
      | [] -> k
      | hd :: tl -> f hd (helper tl k)
    in
    helper
  ;;

  type fork_pos =
    | Right_After
    | Other

  let down_anal (_, _, p) (dead_vars, call_ars) =
    let open IMap in
    let lookup_fin_call_ars t k_none k_found fin_call_ars =
      match t with
      | UVar { id; _ } ->
        (match find id fin_call_ars with
         | res -> k_found res fin_call_ars
         | exception Not_found -> k_none fin_call_ars)
      | _ -> k_none fin_call_ars
    in
    let lookup_call_ars id k_none k_found =
      match IMap.find id call_ars with
      | call_ar -> k_found call_ar
      | exception Not_found -> k_none ()
    in
    let rec anal_p ?(dead_jv_mode = false) p ar k counts =
      match p with
      | Call (_, _, Cont (CPVar { id; _ }, b)) when ISet.mem id dead_vars ->
        anal_p b ar k counts
      | (Call (_, _, CVar _) | Ret (CVar _, _)) when dead_jv_mode -> k ar Other counts
      | Call ((Lam (pat, _, _) as t1), t2, Cont (CPTuple _, b)) ->
        anal_t_bnd pat t2 counts (anal_t t1 (anal_p b ar k))
      | Call ((Lam (pat, i, lam_b) as t1), t2, Cont (CPVar { id; _ }, b)) ->
        let counts' = add id 0 counts in
        anal_t_bnd pat t2 counts' (fun counts'' barriers' fin_call_ars' ->
          let k_none _ = anal_t t1 (anal_p b ar k) counts'' barriers' fin_call_ars' in
          lookup_call_ars i.id k_none (fun call_ar ->
            let k1 rest_ar _ counts''' barriers'' fin_call_ars'' =
              let fin_call_ars''' = add id (call_ar - rest_ar) fin_call_ars'' in
              anal_p b ar k counts''' barriers'' fin_call_ars'''
            in
            lam_hndl ~ar:call_ar pat i lam_b k1 counts'' barriers' fin_call_ars'))
      | Call (Lam (pat, i, lam_b), t2, _) ->
        anal_t_bnd pat t2 counts (lam_hndl ~ar pat i lam_b k)
      | Call (t1, t2, Cont (CPTuple _, b)) ->
        foldd_k anal_t [ t1; t2 ] (anal_p b ar k) counts
      | Call (t1, t2, Cont (CPVar { id; _ }, b)) ->
        let k1 counts' barriers' =
          let counts'' = add id 0 counts' in
          let k_none = anal_p b ar k counts'' barriers' in
          lookup_fin_call_ars t1 k_none (fun t_ar fin_call_ars' ->
            let fin_call_ars'' = add id (Int.max 0 (t_ar - 1)) fin_call_ars' in
            anal_p b ar k counts'' barriers' fin_call_ars'')
        in
        foldd_k anal_t [ t1; t2 ] k1 counts
      | Call (t1, t2, _) ->
        let k1 counts' barriers' =
          lookup_fin_call_ars t1 (k ar Other counts' barriers') (fun t_ar ->
            k (ar - Int.max 0 (t_ar - 1)) Other counts' barriers')
        in
        foldd_k anal_t [ t1; t2 ] k1 counts
      | CIf (t, p1, p2) ->
        let k1 =
          anal_p p1 ar (fun rest_ar1 _ ->
            anal_p p2 ar (fun rest_ar2 _ -> k (Int.max rest_ar1 rest_ar2) Right_After))
        in
        anal_t t k1 counts
      | Let (rec_flag, pat, t, b) -> anal_t_bnd ~rec_flag pat t counts (anal_p b ar k)
      | Ret (Cont (pat, b), t) -> anal_t_bnd pat t counts (anal_p b ar k)
      | Ret (_, Lam (pat, i, lam_b)) ->
        lam_hndl ~ar:(Int.max 0 (ar - 1)) pat i lam_b k counts
      | Ret (_, t) ->
        let k1 counts' barriers' =
          lookup_fin_call_ars t (k 0 Other counts' barriers') (fun t_ar ->
            k (ar - t_ar) Other counts' barriers')
        in
        anal_t t k1 counts
      | Primop (_, _, t, tt, b) -> foldd_k anal_t (t :: tt) (anal_p b ar k) counts
      | Letc (i, Cont (CPVar { id; _ }, cont_b), b) ->
        let jv call_ar =
          let counts' = add id 0 counts in
          let k1 rest_ar _ counts'' barriers' fin_call_ars' =
            let fin_call_ars'' = add id (call_ar - rest_ar) fin_call_ars' in
            anal_p cont_b ar k counts'' barriers' fin_call_ars''
          in
          anal_p ~dead_jv_mode:(ISet.mem i.id dead_vars) b call_ar k1 counts'
        in
        lookup_call_ars i.id (fun _ -> jv 0) jv
      | Letc (_, Cont (CPTuple _, cont_b), b) ->
        anal_p b 0 (fun _ _ -> anal_p cont_b ar k) counts
      | Letc (i, _, b) -> anal_p ~dead_jv_mode:(ISet.mem i.id dead_vars) b ar k counts
    and anal_t_bnd ?(rec_flag = NonRecursive) pat t counts k barriers fin_call_ars =
      match pat with
      | CPVar { id; _ } when ISet.mem id dead_vars -> k counts barriers fin_call_ars
      | CPTuple _ -> anal_t t k counts barriers fin_call_ars
      | CPVar { id; _ } ->
        let counts' = add id 0 counts in
        let k_anal_t _ = anal_t t k counts' barriers fin_call_ars in
        (match t with
         | Lam (pat, i, p) ->
           let rec anal_lam ar k1 =
             let fin_call_ars' =
               match rec_flag with
               | Recursive -> add id ar fin_call_ars
               | NonRecursive -> fin_call_ars
             in
             let k2 rest_ar _ counts'' barriers' fin_call_ars'' =
               if rec_flag = Recursive && rest_ar <> 0
               then anal_lam (ar - rest_ar) k1
               else (
                 let fin_call_ars''' = add id (ar - rest_ar) fin_call_ars'' in
                 k1 counts'' barriers' fin_call_ars''')
             in
             lam_hndl ~ar:(Int.max 0 (ar - 1)) pat i p k2 counts' barriers fin_call_ars'
           in
           lookup_call_ars i.id k_anal_t (fun ar -> anal_lam ar k)
         | t ->
           let k_found t_ar _ =
             let fin_call_ars' = add id t_ar fin_call_ars in
             anal_t t k counts barriers fin_call_ars'
           in
           lookup_fin_call_ars t (fun _ -> k_anal_t ()) k_found fin_call_ars)
    and lam_hndl ?(ar = 0) pat i p k counts =
      let k1 rest_ar fork_pos counts' barriers' fin_call_ars' =
        let barriers'' =
          match fork_pos with
          | Right_After -> ISet.add i.id barriers'
          | Other -> barriers'
        in
        k rest_ar Other counts' barriers'' fin_call_ars'
      in
      let jv = anal_p p ar k1 in
      match pat with
      | CPTuple _ -> jv counts
      | CPVar { id; _ } -> jv (add id 0 counts)
    and anal_t t k counts =
      match t with
      | Lam (pat, i, p) -> lam_hndl pat i p (fun _ _ -> k) counts
      | TSafeBinop (_, t1, t2) -> anal_t t1 (anal_t t2 k) counts
      | UVar { id; _ } ->
        let n = find id counts in
        let counts' = add id (n + 1) counts in
        k counts'
      | TTuple (t1, t2, tt) -> foldd_k anal_t (t1 :: t2 :: tt) k counts
      | TUnit | TConst _ -> k counts
    in
    let k _ _ counts barriers fin_call_ars = dead_vars, counts, barriers, fin_call_ars in
    anal_p p 0 k empty ISet.empty empty
  ;;

  type light_t =
    [ `LightT of triv (* unit size triv*)
    | `Var of var * triv
    ]

  type ex_triv =
    [ light_t
    | `HeavyT of triv
    ]

  type env_elem =
    [ ex_triv
    | `ExLamCall of (pat * var * p) * env_elem * ex_triv list
    | `ExTTCall of env_elem * ex_triv * ex_triv list
    ]

  type ex_call =
    [ `ExLamCall of (pat * var * p) * env_elem * ex_triv list
    | `ExTTCall of env_elem * ex_triv * ex_triv list
    ]

  type etas =
    | Present of ex_triv list
      (* we've add eta-params and we're gonna add due eta-args (or ones substitutions) *)
    | Future of int
  (* jv_case, we're gonna eta expand returned by jv expressions with that number of params and args *)

  type cont_hndl =
    | Def
    | DeadJV
    | Sub of cont * cont_hndl * ex_triv list

  let simpl (rec_flag, pat, p) (dead_vars, counts, barriers, fin_call_ars) =
    let open IMap in
    let prep_t_for_env env t =
      match t with
      | UVar i -> find_with_default i.id env (`Var (i, t))
      | Lam _ | TSafeBinop _ | TTuple _ -> `HeavyT t
      | TConst _ | TUnit -> `LightT t
    in
    let env_var i = `Var (i, UVar i) in
    let gen_eta () =
      let eta = gensym ~prefix:"e" () |> of_string in
      env_var eta, MACPS.CPVar eta
    in
    let gen_many_etas =
      let rec helper args_acc pats_acc n =
        if n < 1
        then args_acc, pats_acc
        else (
          let arg, pat = gen_eta () in
          helper (arg :: args_acc) (pat :: pats_acc) (n - 1))
      in
      helper [] []
    in
    let gen_many_etas_with_k =
      let rec helper args_acc pats_and_k_acc n =
        if n < 1
        then args_acc, pats_and_k_acc
        else (
          let arg, pat = gen_eta () in
          let k = gensym ~prefix:"k" () |> of_string in
          helper (arg :: args_acc) ((pat, k) :: pats_and_k_acc) (n - 1))
      in
      helper [] []
    in
    let v_ar = function
      | CPTuple _ -> 0
      | CPVar { id; _ } -> find_with_default id fin_call_ars 0
    in
    let rec translate_pat = function
      | CPVar i -> MACPS.CPVar i
      | CPTuple (pat1, pat2, pats) ->
        let self = translate_pat in
        MACPS.CPTuple (self pat1, self pat2, List.map self pats)
    in
    let triv_bnd env id_opt prep_t k_inl k_def =
      match id_opt, prep_t with
      | Some id, _ when find id counts <= 1 -> k_inl @@ add id prep_t env
      | Some id, #light_t -> k_inl @@ add id prep_t env
      | _ -> k_def ()
    in
    let rec simpl_p ?(cont_hndl = Def) etas p (env : env_elem IMap.t) =
      let simpl_t_sh = simpl_t env in
      let simpl_p_sh = simpl_p ~cont_hndl etas in
      let get_triv_sh = get_triv env in
      let let_or_ret_bnd pat b t fin_constr =
        let v_ar, id_opt =
          match pat with
          | CPVar { id; _ } -> find_with_default id fin_call_ars 0, Some id
          | CPTuple _ -> 0, None
        in
        let prep_t = prep_t_for_env env t in
        triv_bnd env id_opt prep_t (simpl_p_sh b)
        @@ fun _ ->
        let pat' = translate_pat pat in
        let t' = expand_triv env v_ar prep_t in
        let b' = simpl_p_sh b env in
        fin_constr pat' b' t'
      in
      let ret_or_prep_call ?(etas = etas) call_ar prep_t c get_c' =
        let default ~c ~get_c' = function
          | (#ex_triv as eta) :: etas, `HeavyT (Lam (pat, i, b)) ->
            ex_lam_call pat i b eta etas c get_c' env
          | (#ex_triv as eta) :: etas, (#ex_triv as prep_t) ->
            let t1', t2', tt' = translate_prep_t_tuple env prep_t eta etas in
            MACPS.Call (t1', (t2', tt'), get_c' ())
          | [], (#ex_triv as prep_t) -> Ret (get_c' (), get_triv_sh prep_t)
          | etas, (#ex_call as prep_t) -> ex_call c get_c' env etas prep_t
        in
        let stepped etas diff =
          let new_etas, pats_and_k = gen_many_etas_with_k diff in
          let rec helper = function
            | [] -> failwith "unreachable: diff couldn't be less then 1"
            | (pat', k) :: [] ->
              let b' =
                default
                  ~c:(CVar k)
                  ~get_c':(fun () -> MACPS.CVar k)
                  (etas @ new_etas, prep_t)
              in
              MACPS.Lam ((pat', []), k, b')
            | (pat', k) :: tl -> MACPS.Lam ((pat', []), k, Ret (CVar k, helper tl))
          in
          helper pats_and_k
        in
        match etas with
        | Present pr_etas when call_ar <= List.length pr_etas ->
          default ~c ~get_c' (pr_etas, prep_t)
        | Future n when n <= 0 -> default ~c ~get_c' ([], prep_t)
        | Present pr_etas ->
          MACPS.Ret (get_c' (), stepped pr_etas (call_ar - List.length pr_etas))
        | Future n ->
          let diff = call_ar - n in
          if diff <= 0
          then Ret (get_c' (), expand_triv env n prep_t)
          else (
            let eta_arg, eta_pat = gen_eta () in
            let eta_args, eta_pats = gen_many_etas (n - 1) in
            let k = gensym ~prefix:"k" () |> of_string in
            let b' = MACPS.Ret (CVar k, stepped (eta_arg :: eta_args) diff) in
            MACPS.Ret (get_c' (), Lam ((eta_pat, eta_pats), k, b')))
      in
      let ret ?(call_ar = None) t =
        let call_ar =
          match call_ar, t with
          | Some n, _ -> n
          | None, UVar i -> find_with_default i.id fin_call_ars 0
          | _ -> 0
        in
        let prep_t = prep_t_for_env env t in
        ret_or_prep_call call_ar prep_t
      in
      let call t1 t2 c get_c' =
        let prep_t1, prep_t2 = prep_t_for_env env t1, prep_t_for_env env t2 in
        let call_ar =
          match t1 with
          | UVar i -> find_with_default i.id fin_call_ars 0 - 1
          | _ -> 0
        in
        match prep_t1, prep_t2 with
        | `HeavyT (Lam (lam_pat, i, lam_b)), _ ->
          let elc = `ExLamCall ((lam_pat, i, lam_b), prep_t2, []) in
          ret_or_prep_call call_ar elc c get_c'
        | prep_t1, (#ex_triv as prep_t2) ->
          ret_or_prep_call call_ar (`ExTTCall (prep_t1, prep_t2, [])) c get_c'
        | _, #ex_call ->
          failwith "unreachable: call shouldn't be inlined if its call arity is 0"
      in
      let call_bnd ?(cont_hndl = cont_hndl) ?(extra_args = []) t1 t2 pat b c =
        let v_arity = v_ar pat in
        let t1_prep, t2_prep = prep_t_for_env env t1, prep_t_for_env env t2 in
        let not_inl_ex_call ec =
          let get_c' () = MACPS.Cont (translate_pat pat, simpl_p_sh b env) in
          if v_arity > 0
          then (
            let t' = expand_ex_call env v_arity ec in
            MACPS.Ret (get_c' (), t'))
          else ex_call c get_c' ~cont_stuff:(etas, cont_hndl) env [] ec
        in
        match pat, t1_prep, t2_prep with
        | CPVar { id; _ }, _, (#ex_triv as t2_prep)
          when find id counts <= 1 && v_arity > 0 ->
          let ec =
            match t1_prep with
            | `HeavyT (Lam (lam_pat, i, lam_b)) ->
              `ExLamCall ((lam_pat, i, lam_b), t2_prep, extra_args)
            | _ -> `ExTTCall (t1_prep, t2_prep, extra_args)
          in
          simpl_p_sh b @@ add id ec env
        | _, `HeavyT (Lam (lam_pat, i, lam_b)), _ ->
          not_inl_ex_call @@ `ExLamCall ((lam_pat, i, lam_b), t2_prep, extra_args)
        | _, _, (#ex_triv as t2_prep) ->
          not_inl_ex_call @@ `ExTTCall (t1_prep, t2_prep, extra_args)
        | _, _, #ex_call ->
          failwith "unreachable: call shouldn't be inlined if its call arity is 0"
      in
      match cont_hndl, p with
      | ( _
        , ( Call (_, _, Cont (CPVar { id; _ }, b))
          | Let (_, CPVar { id; _ }, _, b)
          | Ret (Cont (CPVar { id; _ }, b), _) ) )
        when ISet.mem id dead_vars -> simpl_p_sh b env
      | _, Call (t1, t2, (Cont (pat, b) as c)) -> call_bnd t1 t2 pat b c
      | Sub ((Cont (pat, b) as c), cont_hndl, extra_args), Call (t1, t2, _) ->
        call_bnd ~cont_hndl ~extra_args t1 t2 pat b c
      | _, Ret (Cont (pat, b), t) ->
        let_or_ret_bnd pat b t @@ fun pat' b' t' -> MACPS.Ret (Cont (pat', b'), t')
      | _, Let (NonRecursive, pat, t, b) ->
        let_or_ret_bnd pat b t @@ fun pat' b' t' -> MACPS.Let (NonRecursive, pat', t', b')
      | _, Let (Recursive, pat, t, b) ->
        let t' = expand_triv env (v_ar pat) @@ prep_t_for_env env t in
        let b' = simpl_p_sh b env in
        MACPS.Let (Recursive, translate_pat pat, t', b')
      | _, CIf (t, p1, p2) ->
        let t' = simpl_t_sh t in
        let p1', p2' = simpl_p_sh p1 env, simpl_p_sh p2 env in
        CIf (t', p1', p2')
      | _, Primop (pat, i, t1, tt, p) ->
        let pat' = translate_pat pat in
        let t1' = simpl_t_sh t1 in
        let tt' = List.map simpl_t_sh tt in
        let p' = simpl_p_sh p env in
        Primop (pat', i, t1', tt', p')
      | cont_hndl, Letc (i, Cont (pat, b1), b2)
      | Sub (Cont (pat, b1), cont_hndl, _), Letc (i, _, b2) ->
        let pat' = translate_pat pat in
        let b1' = simpl_p ~cont_hndl etas b1 env in
        let cont_hndl, etas2 =
          if ISet.mem i.id dead_vars then DeadJV, Future 0 else Def, Future (v_ar pat)
        in
        let b2' = simpl_p ~cont_hndl etas2 b2 env in
        Letc (i, Cont (pat', b1'), b2')
      | DeadJV, Letc (_, _, b) -> simpl_p ~cont_hndl etas b env
      | (Def | Sub ((CVar _ | HALT), _, _)), Letc (_, c, b) ->
        simpl_p ~cont_hndl:(Sub (c, cont_hndl, [])) etas b env
      | DeadJV, (Ret (CVar i, _) | Call (_, _, CVar i)) -> MACPS.Ret (CVar i, TUnit)
      | Sub ((Cont (pat, b) as c), cont_hndl, e_arg :: e_args), Ret (_, t) ->
        let cont_stuff = etas, cont_hndl in
        let get_c' () = MACPS.Cont (translate_pat pat, simpl_p_sh b env) in
        let ec =
          match prep_t_for_env env t, e_arg with
          | `HeavyT (Lam (lam_pat, i, lam_b)), _ ->
            `ExLamCall ((lam_pat, i, lam_b), (e_arg :> env_elem), [])
          | prep_t1, _ -> `ExTTCall (prep_t1, e_arg, e_args)
        in
        ex_call ~cont_stuff c get_c' env e_args ec
      | Sub (c, cont_hndl, _), Ret (_, t) -> simpl_p ~cont_hndl etas (Ret (c, t)) env
      | Def, Ret ((CVar i as c), t) -> ret t c @@ fun () -> MACPS.CVar i
      | Def, Ret ((HALT as c), t) -> ret t c @@ fun () -> MACPS.HALT
      | Sub (((CVar _ | HALT) as c), cont_hndl, _), Call (t1, t2, _) ->
        simpl_p ~cont_hndl etas (Call (t1, t2, c)) env
      | Def, Call (t1, t2, (CVar i as c)) -> call t1 t2 c @@ fun () -> MACPS.CVar i
      | Def, Call (t1, t2, (HALT as c)) -> call t1 t2 c @@ fun () -> MACPS.HALT
      | DeadJV, (Ret (HALT, _) | Call (_, _, HALT)) ->
        failwith " unreachable: HALT isn't a join value"
    and ex_call ?(cont_stuff = Present [], Def) c get_c' env =
      let rec helper args =
        let triv_triv_call prep_t1 prep_t2 prep_tt =
          let t1', t2', tt' =
            translate_prep_t_tuple env prep_t1 prep_t2 @@ List.append prep_tt args
          in
          MACPS.Call (t1', (t2', tt'), get_c' ())
        in
        function
        | `ExTTCall ((#ex_call as ec), prep_t, prep_tt) ->
          helper (List.cons prep_t @@ prep_tt @ args) ec
        | `ExLamCall ((pat, i, b), prep_t, prep_tt) ->
          ex_lam_call pat i b prep_t (prep_tt @ args) c get_c' env ~cont_stuff
        | `ExTTCall ((#ex_triv as prep_t1), prep_t2, prep_tt) ->
          triv_triv_call prep_t1 prep_t2 prep_tt
      in
      helper
    and translate_prep_t_tuple env prep_t1 prep_t2 prep_tt =
      get_triv env |> fun f -> f prep_t1, f prep_t2, List.map f prep_tt
    and ex_lam_call ?(cont_stuff = Present [], Def) pat i b ex_triv args c get_c' env =
      let is_barrier = ISet.mem i.id barriers in
      let rev_unzip =
        List.fold_left (fun (pats, tt) (pat, t) -> pat :: pats, t :: tt) ([], [])
      in
      let pat' () = translate_pat pat in
      let t' () =
        match pat with
        | CPVar { id; _ } when ISet.mem id dead_vars -> MACPS.TUnit
        | _ -> expand_triv env (v_ar pat) ex_triv
      in
      let addl_rev_pats_tt', args' =
        if not @@ is_barrier
        then [], args
        else
          List.fold_left_map
            (fun acc -> function
               | #light_t as lt -> acc, lt
               | `HeavyT tn ->
                 let eta_arg, eta_pat = gen_eta () in
                 let tn' = simpl_t env tn in
                 (eta_pat, tn') :: acc, eta_arg)
            []
            args
      in
      let pats', tt' = rev_unzip @@ addl_rev_pats_tt' in
      let id_opt =
        match pat with
        | CPVar { id; _ } -> Some id
        | CPTuple _ -> None
      in
      let k_not_fully_inl () =
        let lam' = MACPS.Lam ((pat' (), pats'), i, simpl_p (Present args') b env) in
        MACPS.Call (lam', (t' (), tt'), get_c' ())
      in
      let try_inl k_inl = triv_bnd env id_opt ex_triv k_inl k_not_fully_inl in
      match (pats', tt'), c with
      | ([], []), Cont _ when is_barrier -> k_not_fully_inl ()
      | (pat' :: pats', t' :: tt'), _ ->
        try_inl
        @@ fun env' ->
        let lam' = MACPS.Lam ((pat', pats'), i, simpl_p (Present args') b env') in
        MACPS.Call (lam', (t', tt'), get_c' ())
      | _ ->
        let k_inl =
          let cont_hndl, etas =
            match cont_stuff, c with
            | (etas, cont_hndl), Cont _ -> Sub (c, cont_hndl, args'), etas
            | (_, Sub ((CVar _ | HALT), cont_hndl, eas)), (CVar _ | HALT) ->
              Sub (c, cont_hndl, eas), Present args'
            | (_, cont_hndl), _ -> Sub (c, cont_hndl, []), Present args'
          in
          simpl_p ~cont_hndl etas b
        in
        triv_bnd env id_opt ex_triv k_inl k_not_fully_inl
    and expand_triv env v_ar =
      let expand_lam pat i b etas_count =
        let eta_args, eta_pats = gen_many_etas etas_count in
        let pat' = translate_pat pat in
        let b' = simpl_p (Present eta_args) b env in
        MACPS.Lam ((pat', eta_pats), i, b')
      in
      function
      | `HeavyT (Lam (lam_pat, i, lam_b)) when v_ar > 1 ->
        expand_lam lam_pat i lam_b (v_ar - 1)
      | #ex_call as ec -> expand_ex_call env v_ar ec
      | #ex_triv as et -> get_triv env et
    and expand_ex_call env v_ar ec =
      if v_ar <= 0
      then failwith "unreachable: v_ar should be positive for expansion"
      else (
        let eta_arg, eta_pat = gen_eta () in
        let eta_args, eta_pats = gen_many_etas (v_ar - 1) in
        let i' = gensym ~prefix:"k" () |> of_string in
        let lam_b' =
          ex_call (CVar i') (fun () -> MACPS.CVar i') env (eta_arg :: eta_args) ec
        in
        MACPS.Lam ((eta_pat, eta_pats), i', lam_b'))
    and simpl_t ?(ignore_ids = false) env = function
      | UVar i ->
        if ignore_ids
        then MACPS.UVar i
        else (
          match find i.id env with
          | #ex_triv as et -> get_triv env et
          | #ex_call ->
            failwith "unreachable: call shouldn't be inlined if its call arity is 0 "
          | exception Not_found -> MACPS.UVar i)
      | TSafeBinop (i, t1, t2) -> MACPS.TSafeBinop (i, simpl_t env t1, simpl_t env t2)
      | TConst c -> MACPS.TConst c
      | TUnit -> MACPS.TUnit
      | TTuple (t1, t2, tt) ->
        let smp_sh = simpl_t env in
        MACPS.TTuple (smp_sh t1, smp_sh t2, List.map smp_sh tt)
      | Lam (pat, i, b) ->
        MACPS.Lam ((translate_pat pat, []), i, simpl_p (Present []) b env)
    and get_triv env ex_triv =
      match (ex_triv : ex_triv) with
      | `Var (_, t) | `LightT t | `HeavyT t -> simpl_t ~ignore_ids:true env t
    in
    rec_flag, translate_pat pat, simpl_p (Present []) p empty
  ;;

  let call_arity_anal cps_prog = anal cps_prog |> down_anal cps_prog |> simpl cps_prog

  let call_arity_anal_debug cps_prog =
    let ((_, upw_call_ars) as upw_res) = anal cps_prog in
    Printf.printf "call_ars(after upward anal):\n";
    IMap.iter (Printf.printf "%d: %d\n") upw_call_ars;
    let ((dead_vars, counts, barriers, fin_call_ars) as inter_res) =
      down_anal cps_prog upw_res
    in
    Printf.printf "dead_vars:\n";
    ISet.iter (Printf.printf "%d\n") dead_vars;
    Printf.printf "counts:\n";
    IMap.iter (Printf.printf "%d: %d\n") counts;
    Printf.printf "barriers:\n";
    ISet.iter (Printf.printf "%d\n") barriers;
    Printf.printf "fin_call_ars:\n";
    IMap.iter (Printf.printf "%d: %d\n") fin_call_ars;
    simpl cps_prog inter_res
  ;;
end

open Graph

(* should be changed to custom graph representartion like https://gitlab.haskell.org/ghc/ghc/-/blob/master/compiler/GHC/Data/Graph/UnVar.hs (?) *)
module VeryNaiveCoCallGraph : sig
  type t

  val empty : t
  val adj_nodes : int -> t -> unit IMap.t
  val has_loop : int -> t -> bool
  val cartesian : unit IMap.t -> unit IMap.t -> t
  val cartesian_square : unit IMap.t -> t
  val union : t -> t -> t
  val remove : int -> t -> t
end = struct
  module G = Persistent.Graph.Concrete (struct
      type t = int

      let compare = Stdlib.compare
      let hash = Hashtbl.hash
      let equal = ( = )
      let default = 0
    end)

  type t = G.t

  let empty = G.empty
  let has_loop a g = G.mem_edge g a a
  let remove = Fun.flip G.remove_vertex
  let has_loop v g = G.mem_edge g v v

  let cartesian m1 m2 =
    IMap.fold
      (fun el () (g : t) -> IMap.fold (fun el2 () g -> G.add_edge g el el2) m2 g)
      m1
      G.empty
  ;;

  let cartesian_square m = cartesian m m

  open Oper.P (G)

  let union = union

  open Oper.Neighbourhood (G)

  let adj_nodes v g =
    Base.List.fold ~f:(fun m v -> IMap.add v () m) ~init:IMap.empty
    @@
    match list_from_vertex g v with
    | l -> l
    | exception Invalid_argument _ -> []
  ;;
end

open CallArityAnal (VeryNaiveCoCallGraph)

let test_call_ar_anal cps_prog =
  Format.printf "before:\n%a\n" pp_vb cps_prog;
  call_arity_anal cps_prog |> Format.printf "after:\n%a\n" MACPS.pp_vb;
  ANF.reset_gensym ()
;;

let test_call_ar_anal_debug cps_prog =
  Format.printf "before:\n%a\n" pp_vb cps_prog;
  call_arity_anal_debug cps_prog |> Format.printf "after:\n%a\n" MACPS.pp_vb;
  ANF.reset_gensym ()
;;

let v name = name |> of_string
let prog b = NonRecursive, CPVar (v "main"), b
let var_f, var_g, var_x, var_k1 = v "f", v "g", v "x", v "k1"
let var_y, var_k2, var_h, var_t = v "y", v "k2", v "h", v "t"
let var_a, var_k3, var_b, var_k4 = v "a", v "k3", v "b", v "k4"
let var_l, var_q, var_r, var_k5 = v "l", v "q", v "r", v "k5"
let var_z, var_s, var_d, var_u = v "z", v "s", v "d", v "u"
let var_k6, var_jv1, var_k7 = v "k6", v "jv1", v "k7"
let var_fack, var_fibk, var_n = v "fack", v "fibk", v "n"
let var_k8, var_k, var_m, var_j = v "k8", v "k", v "m", v "j"
let var_v, var_w, var_k9, var_c = v "v", v "w", v "k9", v "c"
let one = TConst (PConst_int 1)
let two = TConst (PConst_int 2)
let tr = TConst (PConst_bool true)
let sum t1 t2 = TSafeBinop ("+" |> of_string, t1, t2)
let le t1 t2 = TSafeBinop ("<=" |> of_string, t1, t2)

let%expect_test "expand call" =
  let th = Call (UVar var_f, one, Cont (CPVar var_g, Call (UVar var_g, one, HALT))) in
  let el = Call (UVar var_f, two, Cont (CPVar var_t, Call (UVar var_t, one, HALT))) in
  let rhs =
    let sum = sum (UVar var_x) (UVar var_y) in
    Lam
      ( CPVar var_x
      , var_k1
      , Ret (CVar var_k1, Lam (CPVar var_y, var_k2, Ret (CVar var_k2, sum))) )
  in
  test_call_ar_anal @@ prog (Let (NonRecursive, CPVar var_f, rhs, CIf (tr, th, el)));
  [%expect
    {|
    before:
    let main = let f x k1 = k1 (fun y k2 -> k2 (x + y)) in if true
                                                                   then f 1
                                                                        (fun g ->

                                                                        g 1
                                                                        (fun x -> x))
                                                                        else
                                                                        f 2
                                                                        (fun t ->

                                                                        t 1
                                                                        (fun x -> x))
    after:

                       let main = let f x e1 k1 = k1 (x + e1) in if true
                                                                 then f 1 1
                                                                      (fun x -> x)
                                                                 else f 2 1
                                                                      (fun x -> x) |}]
;;

let%expect_test "not expand call (one of entries has init arity )" =
  let h_let b =
    Let
      ( NonRecursive
      , CPVar var_h
      , Lam (CPVar var_a, var_k3, Call (UVar var_a, one, CVar var_k3))
      , b )
  in
  let th = Call (UVar var_f, one, Cont (CPVar var_g, Call (UVar var_g, one, HALT))) in
  let el =
    Call (UVar var_f, one, Cont (CPVar var_t, Call (UVar var_h, UVar var_t, HALT)))
  in
  let rhs =
    let sum = TSafeBinop ("+" |> of_string, UVar var_x, UVar var_y) in
    Lam
      ( CPVar var_x
      , var_k1
      , Ret (CVar var_k1, Lam (CPVar var_y, var_k2, Ret (CVar var_k2, sum))) )
  in
  let ite = CIf (tr, th, el) in
  test_call_ar_anal @@ prog (Let (NonRecursive, CPVar var_f, rhs, h_let ite));
  [%expect
    {|
    before:
    let main = let f x k1 = k1 (fun y k2 -> k2 (x + y)) in let h a k3 =
                                                                   a 1 k3
                                                                   in if true
                                                                      then
                                                                      f 1
                                                                      (fun g ->
                                                                       g 1
                                                                       (fun x -> x))
                                                                      else
                                                                      f 1
                                                                      (fun t ->
                                                                       h t
                                                                       (fun x -> x))
    after:

                       let main = let f x k1 = k1 (fun y k2 -> k2 (x + y)) in
                                  if true then f 1 (fun g -> g 1 (fun x -> x))
                                          else f 1 (fun t -> t 1 (fun x -> x)) |}]
;;

let%expect_test "expaned but not fully inlined call (argument is big) " =
  let b =
    let th = Call (UVar var_g, one, Cont (CPVar var_t, Call (UVar var_t, one, HALT))) in
    let el = Call (UVar var_g, one, Cont (CPVar var_b, Call (UVar var_b, one, HALT))) in
    let ite = CIf (tr, th, el) in
    Call (UVar var_f, TTuple (one, one, [ one ]), Cont (CPVar var_g, ite))
  in
  let rhs =
    let tuple = TTuple (UVar var_x, UVar var_x, [ sum (UVar var_y) (UVar var_a) ]) in
    let b =
      Ret
        ( CVar var_k1
        , Lam
            ( CPVar var_y
            , var_k2
            , Ret (CVar var_k2, Lam (CPVar var_a, var_k3, Ret (CVar var_k3, tuple))) ) )
    in
    Lam (CPVar var_x, var_k1, b)
  in
  test_call_ar_anal @@ prog (Let (NonRecursive, CPVar var_f, rhs, b));
  [%expect
    {|
    before:
    let main = let f x k1 = k1 (fun y k2 -> k2 (fun a k3 -> k3 (x, x, y + a)))
                                    in f (1, 1, 1) (fun g -> if true then
                                                                     g 1
                                                                     (fun t ->
                                                                      t 1
                                                                      (fun x -> x))
                                                                     else
                                                                     g 1
                                                                     (fun b ->
                                                                      b 1
                                                                      (fun x -> x)))
    after:

                                      let main = (fun g -> if true then g 1 1
                                                                        (fun x -> x)
                                                                   else g 1 1
                                                                        (fun x -> x))
                                                 (fun e1 e2 k3 -> (fun x k1 ->
                                                                   k1 (x, x, e1 + e2))
                                                                  (1, 1, 1) k3) |}]
;;

let%expect_test "not expand call (because of sharing loses)" =
  let th =
    let cont2 = Cont (CPVar var_q, Ret (HALT, sum (UVar var_q) (UVar var_t))) in
    let cont1 = Cont (CPVar var_t, Call (UVar var_g, one, cont2)) in
    Call (UVar var_f, one, Cont (CPVar var_g, Call (UVar var_g, one, cont1)))
  in
  let el =
    let cont2 = Cont (CPVar var_z, Ret (HALT, sum (UVar var_z) (UVar var_s))) in
    let cont1 = Cont (CPVar var_s, Call (UVar var_g, one, cont2)) in
    Call (UVar var_f, one, Cont (CPVar var_g, Call (UVar var_g, one, cont1)))
  in
  let big_def b =
    Let
      ( NonRecursive
      , CPVar var_h
      , Lam (CPVar var_b, var_k3, Ret (CVar var_k3, sum (UVar var_b) (UVar var_b)))
      , b )
  in
  let rhs =
    let sum = sum (UVar var_l) (UVar var_r) in
    let ret =
      Ret
        ( CVar var_k1
        , Lam
            ( CPVar var_y
            , var_k2
            , Call (UVar var_h, UVar var_y, Cont (CPVar var_r, Ret (CVar var_k2, sum))) )
        )
    in
    let b = big_def (Call (UVar var_h, UVar var_x, Cont (CPVar var_l, ret))) in
    Lam (CPVar var_x, var_k1, b)
  in
  test_call_ar_anal @@ prog (Let (NonRecursive, CPVar var_f, rhs, CIf (tr, th, el)));
  [%expect
    {|
    before:
    let main = let f x k1 = let h b k3 = k3 (b + b) in h x (fun l ->
                                                                    k1 (fun y k2 ->
                                                                        h y
                                                                        (fun r ->

                                                                        k2 (l + r))))
                                                               in if true
                                                                  then f 1
                                                                       (fun g ->
                                                                        g 1
                                                                        (fun t ->

                                                                        g 1
                                                                        (fun q ->

                                                                        (fun x -> x) (q + t))))
                                                                       else
                                                                       f 1
                                                                       (fun g ->
                                                                        g 1
                                                                        (fun s ->

                                                                        g 1
                                                                        (fun z ->

                                                                        (fun x -> x) (z + s))))
    after:

                                                                  let main =
                                                                    let f x k1 =
                                                                    let h b k3 =
                                                                    k3 (b + b)
                                                                    in h x
                                                                       (fun l ->
                                                                        k1
                                                                        (fun y k2 ->

                                                                        h y
                                                                        (fun r ->

                                                                        k2 (l + r))))
                                                                       in
                                                                    if true
                                                                    then
                                                                    f 1 (fun g ->

                                                                        g 1
                                                                        (fun t ->

                                                                        g 1
                                                                        (fun q ->

                                                                        (fun x -> x) (q + t))))
                                                                        else
                                                                        f 1
                                                                        (fun g ->

                                                                        g 1
                                                                        (fun s ->

                                                                        g 1
                                                                        (fun z ->

                                                                        (fun x -> x) (z + s)))) |}]
;;

let%expect_test "expand call (no sharing loses since the variables are dead)" =
  let th =
    let cont2 = Cont (CPVar var_q, Ret (HALT, sum (UVar var_t) (UVar var_t))) in
    let cont1 = Cont (CPVar var_t, Call (UVar var_g, one, cont2)) in
    Call (UVar var_f, one, Cont (CPVar var_g, Call (UVar var_g, one, cont1)))
  in
  let el =
    let cont2 = Cont (CPVar var_z, Ret (HALT, sum (UVar var_s) (UVar var_s))) in
    let cont1 = Cont (CPVar var_s, Call (UVar var_g, one, cont2)) in
    Call (UVar var_f, one, Cont (CPVar var_g, Call (UVar var_g, one, cont1)))
  in
  let big_def b =
    Let
      ( NonRecursive
      , CPVar var_h
      , Lam (CPVar var_b, var_k3, Ret (CVar var_k3, sum (UVar var_b) (UVar var_b)))
      , b )
  in
  let rhs =
    let sum = sum (UVar var_l) (UVar var_r) in
    let ret =
      Ret
        ( CVar var_k1
        , Lam
            ( CPVar var_y
            , var_k2
            , Call (UVar var_h, UVar var_y, Cont (CPVar var_r, Ret (CVar var_k2, sum))) )
        )
    in
    let b = big_def (Call (UVar var_h, UVar var_x, Cont (CPVar var_l, ret))) in
    Lam (CPVar var_x, var_k1, b)
  in
  test_call_ar_anal @@ prog (Let (NonRecursive, CPVar var_f, rhs, CIf (tr, th, el)));
  [%expect
    {|
    before:
    let main = let f x k1 = let h b k3 = k3 (b + b) in h x (fun l ->
                                                                    k1 (fun y k2 ->
                                                                        h y
                                                                        (fun r ->

                                                                        k2 (l + r))))
                                                               in if true
                                                                  then f 1
                                                                       (fun g ->
                                                                        g 1
                                                                        (fun t ->

                                                                        g 1
                                                                        (fun q ->

                                                                        (fun x -> x) (t + t))))
                                                                       else
                                                                       f 1
                                                                       (fun g ->
                                                                        g 1
                                                                        (fun s ->

                                                                        g 1
                                                                        (fun z ->

                                                                        (fun x -> x) (s + s))))
    after:

                                                                  let main =
                                                                    let f x e1 k1 =
                                                                    let h b k3 =
                                                                    k3 (b + b)
                                                                    in h x
                                                                       (fun l ->
                                                                        h e1
                                                                        (fun r ->

                                                                        k1 (l + r)))
                                                                    in if true
                                                                       then
                                                                       f 1 1
                                                                       (fun t ->
                                                                        (fun x -> x) (t + t))
                                                                       else
                                                                       f 1 1
                                                                       (fun s ->
                                                                        (fun x -> x) (s + s)) |}]
;;

let%expect_test "not expand call (because of sharing loses). LamCall version" =
  let b f =
    let cont2 = Cont (CPVar var_q, Ret (HALT, sum (UVar var_q) (UVar var_t))) in
    let cont1 = Cont (CPVar var_t, Call (UVar var_g, one, cont2)) in
    Call (f, sum one one, Cont (CPVar var_g, Call (UVar var_g, one, cont1)))
  in
  let big_def b =
    Let
      ( NonRecursive
      , CPVar var_h
      , Lam (CPVar var_b, var_k3, Ret (CVar var_k3, TTuple (UVar var_x, UVar var_b, [])))
      , b )
  in
  let lam =
    let tuple = TTuple (UVar var_l, UVar var_r, []) in
    let ret =
      Ret
        ( CVar var_k1
        , Lam
            ( CPVar var_y
            , var_k2
            , Call (UVar var_h, UVar var_y, Cont (CPVar var_r, Ret (CVar var_k2, tuple)))
            ) )
    in
    let b = big_def (Call (UVar var_h, UVar var_x, Cont (CPVar var_l, ret))) in
    Lam (CPVar var_x, var_k1, b)
  in
  test_call_ar_anal @@ prog @@ b lam;
  [%expect
    {|
    before:
    let main = (fun x k1 -> let h b k3 = k3 (x, b) in h x (fun l ->
                                                                   k1 (fun y k2 ->
                                                                       h y
                                                                       (fun r ->
                                                                        k2 (l, r))))) (1 + 1)
                                                              (fun g -> g 1
                                                                        (fun t ->

                                                                        g 1
                                                                        (fun q ->

                                                                        (fun x -> x) (q + t))))
    after:

                                                              let main =
                                                                (fun x k1 ->
                                                                 let h b k3 =
                                                                 k3 (x, b)
                                                                 in h x (fun l ->

                                                                        k1
                                                                        (fun y k2 ->

                                                                        h y
                                                                        (fun r ->

                                                                        k2
                                                                        (l, r))))) (1 + 1)
                                                                    (fun g ->
                                                                     g 1
                                                                     (fun t ->
                                                                      g 1
                                                                      (fun q ->
                                                                       (fun x -> x) (q + t)))) |}]
;;

let%expect_test
    "expand call (no sharing loses since the variables are dead). LamCall version"
  =
  let b f =
    let cont2 = Cont (CPVar var_q, Ret (HALT, sum (UVar var_q) (UVar var_q))) in
    let cont1 = Cont (CPVar var_t, Call (UVar var_g, one, cont2)) in
    Call (f, sum one one, Cont (CPVar var_g, Call (UVar var_g, one, cont1)))
  in
  let big_def b =
    Let
      ( NonRecursive
      , CPVar var_h
      , Lam (CPVar var_b, var_k3, Ret (CVar var_k3, TTuple (UVar var_x, UVar var_b, [])))
      , b )
  in
  let lam =
    let tuple = TTuple (UVar var_l, UVar var_r, []) in
    let ret =
      Ret
        ( CVar var_k1
        , Lam
            ( CPVar var_y
            , var_k2
            , Call (UVar var_h, UVar var_y, Cont (CPVar var_r, Ret (CVar var_k2, tuple)))
            ) )
    in
    let b = big_def (Call (UVar var_h, UVar var_x, Cont (CPVar var_l, ret))) in
    Lam (CPVar var_x, var_k1, b)
  in
  test_call_ar_anal @@ prog @@ b lam;
  [%expect
    {|
    before:
    let main = (fun x k1 -> let h b k3 = k3 (x, b) in h x (fun l ->
                                                                   k1 (fun y k2 ->
                                                                       h y
                                                                       (fun r ->
                                                                        k2 (l, r))))) (1 + 1)
                                                              (fun g -> g 1
                                                                        (fun t ->

                                                                        g 1
                                                                        (fun q ->

                                                                        (fun x -> x) (q + q))))
    after:

                                                              let main =
                                                                (fun x k1 ->
                                                                 let h b k3 =
                                                                 k3 (x, b)
                                                                 in h x (fun l ->

                                                                        h 1
                                                                        (fun r ->

                                                                        k1
                                                                        (l, r)))) (1 + 1)
                                                                 (fun q ->
                                                                  (fun x -> x) (q + q)) |}]
;;

let%expect_test " expand call thanks to fake shared comput. and cheap exprs detection " =
  let rhs =
    let sum var2 = sum (UVar var_x) (UVar var2) in
    let th = Ret (CVar var_k1, Lam (CPVar var_y, var_k2, Ret (CVar var_k2, sum var_y))) in
    let el = Ret (CVar var_k1, Lam (CPVar var_a, var_k3, Ret (CVar var_k3, sum var_a))) in
    Lam (CPVar var_x, var_k1, CIf (tr, th, el))
  in
  let th =
    let cont2 = Cont (CPVar var_q, Ret (HALT, sum (UVar var_t) (UVar var_q))) in
    let cont1 = Cont (CPVar var_t, Call (UVar var_g, one, cont2)) in
    Call (UVar var_f, one, Cont (CPVar var_g, Call (UVar var_g, one, cont1)))
  in
  let el =
    let cont2 = Cont (CPVar var_z, Ret (HALT, sum (UVar var_s) (UVar var_z))) in
    let cont1 = Cont (CPVar var_s, Call (UVar var_g, one, cont2)) in
    Call (UVar var_f, one, Cont (CPVar var_g, Call (UVar var_g, one, cont1)))
  in
  test_call_ar_anal @@ prog (Let (NonRecursive, CPVar var_f, rhs, CIf (tr, th, el)));
  [%expect
    {|
    before:
    let main = let f x k1 = if true then k1 (fun y k2 -> k2 (x + y))
                                            else k1 (fun a k3 -> k3 (x + a))
                                    in if true then f 1 (fun g -> g 1 (fun t ->
                                                                       g 1
                                                                       (fun q ->
                                                                        (fun x -> x) (t + q))))
                                                                 else f 1
                                                                      (fun g ->
                                                                       g 1
                                                                       (fun s ->
                                                                        g 1
                                                                        (fun z ->

                                                                        (fun x -> x) (s + z))))
    after:

                                               let main = let f x e1 k1 =
                                                          if true then k1 (x + e1)
                                                                  else k1 (x + e1)
                                                          in if true then
                                                                     (fun g ->
                                                                      g 1
                                                                      (fun t ->
                                                                       g 1
                                                                       (fun q ->
                                                                        (fun x -> x) (t + q))))
                                                                      (fun e4 k5 ->
                                                                       f 1 e4 k5)
                                                                      else
                                                                      (fun g ->
                                                                       g 1
                                                                       (fun s ->
                                                                        g 1
                                                                        (fun z ->

                                                                        (fun x -> x) (s + z))))
                                                                       (fun e2 k3 ->
                                                                        f 1 e2 k3) |}]
;;

let%expect_test "expand lam call argument" =
  let rhs =
    let b =
      Ret
        ( CVar var_k1
        , Lam (CPVar var_y, var_k2, Ret (CVar var_k2, TTuple (UVar var_x, UVar var_y, [])))
        )
    in
    Lam (CPVar var_x, var_k1, b)
  in
  let f =
    Lam
      ( CPVar var_h
      , var_k3
      , Call (UVar var_h, one, Cont (CPVar var_s, Call (UVar var_s, one, CVar var_k3))) )
  in
  let th =
    Call
      ( f
      , Lam
          ( CPVar var_t
          , var_k4
          , Call (UVar var_f, TTuple (one, UVar var_t, []), CVar var_k4) )
      , HALT )
  in
  let el =
    Call
      (UVar var_f, TTuple (one, one, []), Cont (CPVar var_g, Call (UVar var_g, one, HALT)))
  in
  test_call_ar_anal @@ prog (Let (NonRecursive, CPVar var_f, rhs, CIf (tr, th, el)));
  [%expect
    {|
    before:
    let main = let f x k1 = k1 (fun y k2 -> k2 (x, y)) in if true
                                                                  then (fun h k3 ->
                                                                        h 1
                                                                        (fun s ->

                                                                        s 1 k3))
                                                                       (fun t k4 ->
                                                                        f
                                                                        (1, t) k4)
                                                                       (fun x -> x)
                                                                       else
                                                                       f
                                                                       (1, 1)
                                                                       (fun g ->
                                                                        g 1
                                                                        (fun x -> x))
    after:

                                                                  let main =
                                                                    let f x e1 k1 =
                                                                    k1 (x, e1)
                                                                    in if true
                                                                       then
                                                                       f
                                                                       (1, 1) 1
                                                                       (fun x -> x)
                                                                       else
                                                                       f
                                                                       (1, 1) 1
                                                                       (fun x -> x) |}]
;;

let%expect_test "expand but not inl lam call argument" =
  let rhs =
    let b =
      Ret
        ( CVar var_k1
        , Lam (CPVar var_y, var_k2, Ret (CVar var_k2, TTuple (UVar var_x, UVar var_y, [])))
        )
    in
    Lam (CPVar var_x, var_k1, b)
  in
  let f =
    let th =
      Call (UVar var_h, one, Cont (CPVar var_s, Call (UVar var_s, one, CVar var_k3)))
    in
    let el =
      Call (UVar var_h, one, Cont (CPVar var_q, Call (UVar var_q, one, CVar var_k3)))
    in
    Lam (CPVar var_h, var_k3, CIf (tr, th, el))
  in
  let th =
    Call
      ( f
      , Lam
          ( CPVar var_t
          , var_k4
          , Call (UVar var_f, TTuple (one, UVar var_t, []), CVar var_k4) )
      , HALT )
  in
  let el =
    Call
      (UVar var_f, TTuple (one, one, []), Cont (CPVar var_g, Call (UVar var_g, one, HALT)))
  in
  test_call_ar_anal @@ prog (Let (NonRecursive, CPVar var_f, rhs, CIf (tr, th, el)));
  [%expect
    {|
    before:
    let main = let f x k1 = k1 (fun y k2 -> k2 (x, y)) in if true
                                                                  then (fun h k3 ->
                                                                        if true
                                                                        then
                                                                        h 1
                                                                        (fun s ->

                                                                        s 1 k3)
                                                                        else
                                                                        h 1
                                                                        (fun q ->

                                                                        q 1 k3))
                                                                        (fun t k4 ->

                                                                        f
                                                                        (1, t) k4)
                                                                        (fun x -> x)
                                                                        else
                                                                        f
                                                                        (1, 1)
                                                                        (fun g ->

                                                                        g 1
                                                                        (fun x -> x))
    after:

                                                                  let main =
                                                                    let f x e1 k1 =
                                                                    k1 (x, e1)
                                                                    in if true
                                                                       then
                                                                       (fun h k3 ->
                                                                        if true
                                                                        then
                                                                        h 1 1 k3
                                                                        else
                                                                        h 1 1 k3)
                                                                       (fun t e2 k4 ->
                                                                        f
                                                                        (1, t) e2 k4)
                                                                       (fun x -> x)
                                                                       else
                                                                       f
                                                                       (1, 1) 1
                                                                       (fun x -> x) |}]
;;

let%expect_test "expand jv" =
  let sum1 = sum (UVar var_x) (UVar var_y) in
  let sum2 = sum (UVar var_l) (UVar var_r) in
  let th =
    Ret
      ( CVar var_jv1
      , Lam
          ( CPVar var_x
          , var_k1
          , Ret (CVar var_k1, Lam (CPVar var_y, var_k2, Ret (CVar var_k2, sum1))) ) )
  in
  let el =
    Ret
      ( CVar var_jv1
      , Lam
          ( CPVar var_l
          , var_k3
          , Ret (CVar var_k1, Lam (CPVar var_r, var_k4, Ret (CVar var_k2, sum2))) ) )
  in
  let cont =
    Cont
      ( CPVar var_f
      , Call (UVar var_f, one, Cont (CPVar var_g, Call (UVar var_g, one, HALT))) )
  in
  test_call_ar_anal @@ prog (Letc (var_jv1, cont, CIf (tr, th, el)));
  [%expect
    {|
    before:
    let main = let jv1 f = f 1 (fun g -> g 1 (fun x -> x)) in if true
                                                                      then
                                                                      jv1
                                                                      (fun x k1 ->
                                                                       k1
                                                                       (fun y k2 ->
                                                                        k2 (x + y)))
                                                                      else
                                                                      jv1
                                                                      (fun l k3 ->
                                                                       k1
                                                                       (fun r k4 ->
                                                                        k2 (l + r)))
    after:

                                                                      let main =
                                                                        let jv1 f =
                                                                        f 1 1
                                                                        (fun x -> x)
                                                                        in
                                                                        if true
                                                                        then
                                                                        jv1
                                                                        (fun x e2 k1 ->

                                                                        k1 (x + e2))
                                                                        else
                                                                        jv1
                                                                        (fun l e1 k3 ->

                                                                        k1 (l + e1)) |}]
;;

let%expect_test "dead jv param" =
  let sum1 = sum (UVar var_x) (UVar var_y) in
  let sum2 = sum (UVar var_l) (UVar var_r) in
  let th =
    Ret
      ( CVar var_jv1
      , Lam
          ( CPVar var_x
          , var_k1
          , Ret (CVar var_k1, Lam (CPVar var_y, var_k2, Ret (CVar var_k2, sum1))) ) )
  in
  let el =
    Ret
      ( CVar var_jv1
      , Lam
          ( CPVar var_l
          , var_k3
          , Ret (CVar var_k1, Lam (CPVar var_r, var_k4, Ret (CVar var_k2, sum2))) ) )
  in
  let cont = Cont (CPVar var_f, Ret (HALT, sum one one)) in
  test_call_ar_anal @@ prog (Letc (var_jv1, cont, CIf (tr, th, el)));
  [%expect
    {|
    before:
    let main = let jv1 f = (fun x -> x) (1 + 1) in if true then jv1
                                                                        (fun x k1 ->

                                                                        k1
                                                                        (fun y k2 ->

                                                                        k2 (x + y)))
                                                                        else
                                                                        jv1
                                                                        (fun l k3 ->

                                                                        k1
                                                                        (fun r k4 ->

                                                                        k2 (l + r)))
    after:

                                                           let main = let jv1 f =
                                                                      (fun x -> x) (1 + 1)
                                                                      in
                                                                      if true
                                                                      then
                                                                      jv1 ()
                                                                      else
                                                                      jv1 () |}]
;;

let%expect_test
    "barriers in action: only unit-size arguments are inlined when eta-param entry count \
     > 1"
  =
  let big_def b =
    let lam_b =
      Ret
        ( CVar var_k3
        , Lam (CPVar var_b, var_k4, Ret (CVar var_k4, TTuple (UVar var_a, UVar var_b, [])))
        )
    in
    Let (NonRecursive, CPVar var_h, Lam (CPVar var_a, var_k3, lam_b), b)
  in
  let rhs =
    let ite =
      CIf (UVar var_y, Ret (CVar var_k2, UVar var_h), Ret (CVar var_k2, UVar var_h))
    in
    let b = Ret (CVar var_k1, Lam (CPVar var_y, var_k2, ite)) in
    Lam (CPVar var_x, var_k1, big_def b)
  in
  let cont2 =
    Cont
      ( CPVar var_g
      , Call
          ( UVar var_g
          , TTuple (one, two, [])
          , Cont (CPVar var_t, Call (UVar var_t, two, HALT)) ) )
  in
  let cont1 = Cont (CPVar var_r, Call (UVar var_r, le two one, cont2)) in
  let b = Call (UVar var_f, TUnit, cont1) in
  test_call_ar_anal @@ prog @@ Let (NonRecursive, CPVar var_f, rhs, b);
  [%expect
    {|
    before:
    let main = let f x k1 = let h a k3 = k3 (fun b k4 -> k4 (a, b)) in
                                    k1 (fun y k2 -> if y then k2 h else k2 h)
                                    in f () (fun r -> r (2 <= 1) (fun g ->
                                                                  g (1, 2)
                                                                  (fun t ->
                                                                   t 2 (fun x -> x))))
    after:

                                      let main = let h a e1 k3 = k3 (a, e1) in
                                                 (fun e2 k2 -> if 2 <= 1
                                                               then h e2 2 k2
                                                               else h e2 2 k2)
                                                 (1, 2) (fun x -> x) |}]
;;

let%expect_test "fack" =
  let le_sign, min_sign = "<=" |> of_string, "-" |> of_string in
  let th =
    Ret (CVar var_k1, Lam (CPVar var_k, var_k2, Call (UVar var_k, one, CVar var_k2)))
  in
  let mult = TSafeBinop ("*" |> of_string, UVar var_t, UVar var_n) in
  let el =
    let cont2 = Cont (CPVar var_t, Call (UVar var_k, mult, CVar var_k3)) in
    let cont =
      Cont
        ( CPVar var_h
        , Ret
            (CVar var_k1, Lam (CPVar var_k, var_k3, Call (UVar var_h, UVar var_k, cont2)))
        )
    in
    Call (UVar var_fack, TSafeBinop (min_sign, UVar var_n, one), cont)
  in
  let rhs =
    Lam (CPVar var_n, var_k1, CIf (TSafeBinop (le_sign, UVar var_n, one), th, el))
  in
  let id = Lam (CPVar var_x, var_k4, Ret (CVar var_k4, UVar var_x)) in
  let sec_arg_app = Call (UVar var_g, id, HALT) in
  test_call_ar_anal
  @@ prog
  @@ Let
       ( Recursive
       , CPVar var_fack
       , rhs
       , Call (UVar var_fack, one, Cont (CPVar var_g, sec_arg_app)) );
  [%expect
    {|
    before:
    let main = let rec fack n k1 = if n <= 1 then k1 (fun k k2 -> k 1 k2)
                                                     else fack (n - 1) (fun h ->
                                                                        k1
                                                                        (fun k k3 ->

                                                                        h k
                                                                        (fun t ->

                                                                        k (t * n) k3)))
                                                     in fack 1 (fun g ->
                                                                        g
                                                                        (fun x k4 ->

                                                                        k4 x)
                                                                        (fun x -> x))
    after:

                                           let main = let rec fack n e1 k1 =
                                                      if n <= 1 then e1 1 k1
                                                      else fack (n - 1) e1
                                                           (fun t -> e1 (t * n) k1)
                                                        in fack 1 (fun x k4 ->
                                                                   k4 x)
                                                                  (fun x -> x) |}]
;;

let%expect_test "fibk" =
  test_call_ar_anal
  @@ Result.get_ok
  @@ cps_conv_program
  @@ Result.get_ok
  @@ Frontend.Parsing.parse_structure
       "let main  = let rec fibk n = if n <=1 then fun k -> k 1 else let h = fibk (n-1) \
        in fun k -> h (fun l -> fibk (n-2) (fun r -> k (l + r)) ) in fibk 1 (fun x -> x)";
  [%expect
    {|
    before:
    let main = let rec fibk n k1 = if n <= 1 then k1 (fun k k2 -> k 1 k2)
                                                     else fibk (n - 1) (fun t3 ->
                                                                        k1
                                                                        (fun k k4 ->

                                                                        t3
                                                                        (fun l k5 ->

                                                                        fibk (n - 2)
                                                                        (fun t6 ->

                                                                        t6
                                                                        (fun r k7 ->

                                                                        k (l + r) k7) k5)) k4))
                                                                       in
                                                          fibk 1 (fun t8 ->
                                                                  t8 (fun x k9 ->
                                                                      k9 x)
                                                                     (fun t10 ->
                                                                      (fun x -> x) t10))
    after:

                                                          let main = let rec fibk n e11 k1 =
                                                                     if n <= 1
                                                                     then
                                                                     e11 1 k1
                                                                     else
                                                                     fibk (n - 1)
                                                                     (fun l k5 ->
                                                                      fibk (n - 2)
                                                                      (fun r k7 ->
                                                                       e11 (l + r) k7) k5) k1
                                                                     in fibk 1
                                                                        (fun x k9 ->

                                                                        k9 x)
                                                                        (fun t10 ->

                                                                        (fun x -> x) t10) |}]
;;

let%expect_test "branches with diff fin_call_ars" =
  let rhs1 =
    let big_def b =
      Let
        ( NonRecursive
        , CPVar var_h
        , Lam (CPVar var_b, var_k4, Ret (CVar var_k4, sum (UVar var_b) (UVar var_b)))
        , b )
    in
    let sum = sum (UVar var_z) @@ sum (UVar var_l) (UVar var_r) in
    let ret2 =
      Ret
        ( CVar var_k2
        , Lam
            ( CPVar var_y
            , var_k3
            , Call (UVar var_h, UVar var_y, Cont (CPVar var_r, Ret (CVar var_k3, sum))) )
        )
    in
    let ret = Ret (CVar var_k1, Lam (CPVar var_z, var_k2, ret2)) in
    let b = big_def (Call (UVar var_h, UVar var_x, Cont (CPVar var_l, ret))) in
    Lam (CPVar var_x, var_k1, b)
  in
  let rhs2 =
    let big_def b =
      Let
        ( NonRecursive
        , CPVar var_u
        , Lam (CPVar var_a, var_k8, Ret (CVar var_k8, sum (UVar var_a) (UVar var_a)))
        , b )
    in
    let sum = sum (UVar var_j) @@ sum (UVar var_n) (UVar var_d) in
    let ret2 =
      Ret
        ( CVar var_k6
        , Lam
            ( CPVar var_c
            , var_k7
            , Call (UVar var_u, UVar var_c, Cont (CPVar var_d, Ret (CVar var_k7, sum))) )
        )
    in
    let ret = Ret (CVar var_k5, Lam (CPVar var_j, var_k6, ret2)) in
    let b = big_def (Call (UVar var_u, UVar var_s, Cont (CPVar var_n, ret))) in
    Lam (CPVar var_s, var_k5, b)
  in
  let let_f =
    let th = Call (UVar var_q, UVar var_t, CVar var_k9) in
    let el = Call (UVar var_g, UVar var_t, CVar var_k9) in
    let lam_b = CIf (tr, th, el) in
    let b =
      let cont = Cont (CPVar var_m, Ret (HALT, TTuple (UVar var_m, UVar var_g, []))) in
      Call
        ( UVar var_f
        , one
        , Cont
            ( CPVar var_v
            , Call (UVar var_v, one, Cont (CPVar var_w, Call (UVar var_w, one, cont))) )
        )
    in
    Let (Recursive, CPVar var_f, Lam (CPVar var_t, var_k9, lam_b), b)
  in
  let let_q = Let (Recursive, CPVar var_q, rhs2, let_f) in
  test_call_ar_anal @@ prog (Let (NonRecursive, CPVar var_g, rhs1, let_q));
  [%expect
    {|
    before:
    let main = let g x k1 = let h b k4 = k4 (b + b) in h x (fun l ->
                                                                    k1 (fun z k2 ->
                                                                        k2
                                                                        (fun y k3 ->

                                                                        h y
                                                                        (fun r ->

                                                                        k3 (z + (l + r))))))
                                                               in let rec q s k5 =
                                                                  let u a k8 =
                                                                  k8 (a + a)
                                                                  in u s
                                                                     (fun n ->
                                                                      k5
                                                                      (fun j k6 ->
                                                                       k6
                                                                       (fun c k7 ->
                                                                        u c
                                                                        (fun d ->

                                                                        k7 (j + (n + d))))))
                                                                     in let rec f t k9 =
                                                                        if true
                                                                        then
                                                                        q t k9
                                                                        else
                                                                        g t k9
                                                                        in
                                                                        f 1
                                                                        (fun v ->

                                                                        v 1
                                                                        (fun w ->

                                                                        w 1
                                                                        (fun m ->

                                                                        (fun x -> x)
                                                                        (m, g))))
    after:

                                                                    let main =
                                                                      let g x k1 =
                                                                      let h b k4 =
                                                                      k4 (b + b)
                                                                      in
                                                                      h x
                                                                      (fun l ->
                                                                       k1
                                                                       (fun z k2 ->
                                                                        k2
                                                                        (fun y k3 ->

                                                                        h y
                                                                        (fun r ->

                                                                        k3 (z + (l + r))))))
                                                                      in
                                                                      let rec q s e2 e1 k5 =
                                                                      let u a k8 =
                                                                      k8 (a + a)
                                                                      in
                                                                      u s
                                                                      (fun n ->
                                                                       u e1
                                                                       (fun d ->
                                                                        k5 (e2 + (n + d))))
                                                                      in
                                                                        let rec f t k9 =
                                                                        if true
                                                                        then
                                                                        k9
                                                                        (fun e5 k6 ->

                                                                        k6
                                                                        (fun e3 k4 ->

                                                                        q t e5 e3 k4))
                                                                        else
                                                                        g t k9
                                                                        in
                                                                        f 1
                                                                        (fun v ->

                                                                        v 1
                                                                        (fun w ->

                                                                        w 1
                                                                        (fun m ->

                                                                        (fun x -> x)
                                                                        (m, g)))) |}]
;;

let%expect_test "dead lam_call_bnd lam par. near the barrier" =
  let lam =
    Lam (CPVar var_x, var_k1, CIf (tr, Ret (CVar var_k1, one), Ret (CVar var_k1, two)))
  in
  let arg_lam =
    let lam2 =
      Lam (CPVar var_b, var_k4, Ret (CVar var_k4, TTuple (UVar var_a, UVar var_b, [])))
    in
    Lam (CPVar var_a, var_k3, Ret (CVar var_k3, lam2))
  in
  let call =
    Call
      (lam, arg_lam, Cont (CPVar var_y, Ret (HALT, TTuple (UVar var_y, UVar var_y, []))))
  in
  test_call_ar_anal @@ prog call;
  [%expect
    {|
    before:
    let main = (fun x k1 -> if true then k1 1 else k1 2) (fun a k3 ->
                                                                  k3 (fun b k4 ->
                                                                      k4 (a, b)))
                                                                 (fun y ->
                                                                  (fun x -> x)
                                                                  (y, y))
    after:

                       let main = (fun x k1 -> if true then k1 1 else k1 2) ()
                                  (fun y -> (fun x -> x) (y, y)) |}]
;;

type cps1_vb = OneACPS.cps_vb

let cps1_vb_to_parsetree_vb = OneACPS.cps_vb_to_parsetree_vb
let pp_cps1_vb = OneACPS.pp_vb

type cpsm_vb = MACPS.cps_vb

let cpsm_vb_to_parsetree_vb = MACPS.cps_vb_to_parsetree_vb
let pp_cpsm_vb = MACPS.pp_vb
let call_arity_anal = call_arity_anal
