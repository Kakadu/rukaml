(* https://www.khoury.northeastern.edu/home/shivers/papers/nobrainer-cps.pdf *)

open Miniml.Parsetree
open Miniml.Ident

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

let devar i = DEVar i
let deite c t e = DEIf (c, t, e)
let deconst n = DEConst n
let delam v body = DELam (v, body)
let deapp1 f x = DEApp (f, x)
let dpvar i = DPVar i
let detuple a b xs = DETuple (a, b, xs)
let dptuple a b xs = DPTuple (a, b, xs)

module SMap = Map.Make (String)
module IMap = Map.Make (Int)
module ISet = Set.Make (Int)

type var = ident

let gensym = ANF.gensym_s

(* Syntax of CPS target language *)
type pat =
  | CPVar of var
  | CPTuple of pat * pat * pat list

type p =
  | Call of triv * triv * cont
  | Ret of cont * returnable
  | CIf of triv * p * p
  | Let of rec_flag * pat * triv * p
  | Letc of var * cont * p

and returnable =
  | Triv of triv
  | Print of triv

and cont =
  | Cont of pat * p
  | CVar of var
  | HALT

and tuple = triv * triv * triv list
and primbinop = var * triv * triv

and triv =
  | Lam of pat * var * p
  | PrimBinop of primbinop
  | UVar of var
  | TConst of const
  | TTuple of tuple
  | TUnit

(* Abstract args *)
type a =
  | AVar of var
  | AClo of ds_pattern * ds_expr * env
  | AConst of const
  | AUnit
  | ATuple of tuple
  | APrimBinop of primbinop

and env = a IMap.t

(* Abstract continuations *)
type c =
  | AHALT
  | KVar of var
  | FCont of ds_expr * env * c
  | ACont of a * c
  | ICont of ds_expr * ds_expr * env * c
  | TupleBldCont of ds_expr list * triv list * env * c
  | LetRecCont of pat * ds_expr * env * c
  | ToplevelLetRecCont of pat * ds_vb list
  | LetNonRecCont of ds_pattern * ds_expr * env * c
  | ToplevelLetNonRecCont of ds_pattern * ds_vb list
  | BinopsFirstArgCont of var * ds_expr * env * c
  | BinopsSecondArgCont of var * triv * c

(* Extend a static environment with a new [y |-> a] entry. *)
let extend y a env = IMap.add y a env

(* Utilities to maintain reference counts of user vars in CPS term. *)
let new_count x counts = IMap.add x 0 counts
let incr x counts = IMap.add x (1 + IMap.find x counts) counts

let distr_ids_helper ?(with_printing = false) (rec_flag, ptrn, expr) glob_vars k =
  let open SMap in
  let rec helper_e vars node k =
    let rec helper_ee vars' k' node =
      match node with
      | [] -> k' []
      | hd :: tl ->
        helper_e vars' hd (fun _ np -> helper_ee vars' (fun nn -> k' (np :: nn)) tl)
    in
    match node with
    | EUnit -> k vars DEUnit
    | EConst x -> k vars (deconst x)
    | EVar hum_name ->
      fun free_vars ->
        (match
           Base.Option.first_some (find_opt hum_name vars) (find_opt hum_name free_vars)
         with
         | None ->
           let ident = of_string hum_name in
           k
             (add ident.hum_name ident.id vars)
             (devar { hum_name; id = ident.id })
             (add hum_name ident.id free_vars)
         | Some id -> k vars (devar { hum_name; id }) free_vars)
    | EIf (e1, e2, e3) ->
      (fun _ n1 ->
        helper_e vars e2 (fun _ n2 ->
          helper_e vars e3 (fun _ n3 -> deite n1 n2 n3 |> k vars)))
      |> helper_e vars e1
    | ELam (p, e) ->
      (fun v np -> helper_e v e (fun _ n -> delam np n |> k vars)) |> helper_p p vars
    | EApp (e1, e2) ->
      (fun _ n1 -> helper_e vars e2 (fun _ n2 -> deapp1 n1 n2 |> k vars))
      |> helper_e vars e1
    | ETuple (e1, e2, ee) ->
      (fun _ n1 ->
        helper_e vars e2 (fun _ n2 ->
          helper_ee vars (fun nn fv3 -> (detuple n1 n2 nn |> k vars) fv3) ee))
      |> helper_e vars e1
    | ELet (NonRecursive, p, e1, e2) ->
      (fun _ n1 ->
        helper_p p vars (fun v np ->
          helper_e v e2 (fun _ n2 -> DELet (NonRecursive, np, n1, n2) |> k vars)))
      |> helper_e vars e1
    | ELet (Recursive, p, e1, e2) ->
      (fun v np ->
        helper_e v e1 (fun _ n1 ->
          helper_e v e2 (fun _ n2 -> DELet (Recursive, np, n1, n2) |> k vars)))
      |> helper_p p vars
  and helper_p p vars k =
    match p with
    | PVar s ->
      let ident = of_string s in
      if with_printing then Printf.printf "var %s got id %d\n" s ident.id;
      k (add ident.hum_name ident.id vars) (dpvar ident)
    | PTuple (p1, p2, pp) ->
      (fun v np1 ->
        helper_p p2 v (fun v' np2 ->
          helper_pp v' (fun v'' npp -> dptuple np1 np2 npp |> k v'') pp))
      |> helper_p p1 vars
  and helper_pp vars k = function
    | [] -> k vars []
    | hd :: tl ->
      helper_p hd vars (fun v np -> helper_pp v (fun v' nn -> k v' (np :: nn)) tl)
  in
  match rec_flag with
  | Recursive ->
    (fun gv ds_ptrn ->
      helper_e gv expr (fun _ ds_expr -> (rec_flag, ds_ptrn, ds_expr) |> k gv))
    |> helper_p ptrn glob_vars
  | NonRecursive ->
    (fun _ ds_expr ->
      helper_p ptrn glob_vars (fun gv ds_ptrn -> (rec_flag, ds_ptrn, ds_expr) |> k gv))
    |> helper_e glob_vars expr
;;

let start_glob_envs =
  let extend v =
    let ident = Miniml.Ident.of_string v in
    SMap.add ident.hum_name ident.id
  in
  let printi = Miniml.Ident.of_string "print" in
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

let distr_ids_vb ?(with_printing = false) vb =
  distr_ids_helper
    ~with_printing
    vb
    (snd start_glob_envs)
    (fun _ ds_vb free_vars ->
      if SMap.is_empty free_vars then Result.ok ds_vb else Result.error free_vars)
    SMap.empty
;;

let distr_ids_program ?(with_printing = false) =
  let rec helper glob_vars k free_vars = function
    | hd :: tl ->
      distr_ids_helper
        ~with_printing
        hd
        glob_vars
        (fun gv ds_vb fv -> helper gv (fun ds_vbs fv2 -> k (ds_vb :: ds_vbs) fv2) fv tl)
        free_vars
    | [] -> k [] free_vars
  in
  helper
    (snd start_glob_envs)
    (fun ds_vb free_vars ->
      if SMap.is_empty free_vars then Result.ok ds_vb else Result.error free_vars)
    SMap.empty
;;

let preconv_count_helper (_, p, e) containers =
  let rec helper_e ((counts, ref_once, no_refs) as cntrs) node k =
    let open IMap in
    let upd id =
      let n = find id counts in
      ( add id (n + 1) counts
      , (match n with
         | 0 -> ISet.add id ref_once
         | 1 -> ISet.remove id ref_once
         | _ -> ref_once)
      , if n == 0 then ISet.remove id no_refs else no_refs )
      |> k
    in
    let rec helper_list f cntrs' k' = function
      | [] -> k' cntrs'
      | hd :: tl -> (fun c -> helper_list f c k' tl) |> f cntrs' hd
    in
    let rec helper_p ((counts', ref_once', no_refs') as cntrs) node k' =
      match node with
      | DPVar i -> (add i.id 0 counts', ref_once', ISet.add i.id no_refs') |> k'
      | DPTuple (p1, p2, pp) -> helper_list helper_p cntrs k' (p1 :: p2 :: pp)
    in
    match node with
    | DEUnit | DEConst _ -> k cntrs
    | DEVar i ->
      if i.hum_name |> fun h -> ANF.is_infix_binop h || String.equal "print" h
      then k cntrs
      else upd i.id
    | DELam (p, e) -> (fun c -> helper_e c e k) |> helper_p cntrs p
    | DEApp (e1, e2) -> helper_list helper_e cntrs k [ e1; e2 ]
    | DELet (_, p, e1, e2) ->
      (fun c -> helper_list helper_e c k [ e1; e2 ]) |> helper_p cntrs p
    | DEIf (e1, e2, e3) -> helper_list helper_e cntrs k [ e1; e2; e3 ]
    | DETuple (e1, e2, ee) -> helper_list helper_e cntrs k (e1 :: e2 :: ee)
  and helper_list f cntrs' k' = function
    | [] -> k' cntrs'
    | hd :: tl -> (fun c -> helper_list f c k' tl) |> f cntrs' hd
  and helper_p ((counts, ref_once, no_refs) as cntrs) node k' =
    let open IMap in
    match node with
    | DPVar i -> (add i.id 0 counts, ref_once, ISet.add i.id no_refs) |> k'
    | DPTuple (p1, p2, pp) -> helper_list helper_p cntrs k' (p1 :: p2 :: pp)
  in
  fun k -> (fun c -> helper_e c e k) |> helper_p containers p
;;

let preconv_count_vb vb =
  let open ISet in
  preconv_count_helper vb (IMap.empty, empty, empty) Fun.id
;;

let preconv_count_program =
  let open IMap in
  let rec helper cntrs = function
    | hd :: tl -> preconv_count_helper hd cntrs (fun cntrs -> helper cntrs tl)
    | [] -> cntrs
  in
  helper (empty, ISet.empty, ISet.empty)
;;

let test_count text =
  match Miniml.Parsing.parse_structure text with
  | Result.Ok stru ->
    let ds_stru = Result.get_ok (distr_ids_program ~with_printing:true stru) in
    let counts, ref_once, no_refs = preconv_count_program ds_stru in
    IMap.iter (Printf.printf "id: %d; counts %d\n") counts;
    Printf.printf "ids that ref_once:\n";
    ISet.iter (Printf.printf "%d\n") ref_once;
    Printf.printf "ids that no_refs:\n";
    ISet.iter (Printf.printf "%d\n") no_refs;
    ANF.reset_gensym ()
  | _ -> Format.printf "parsing error\n"
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
      List.fold_left
        (fun (cpp, env, counts) pn ->
          extend_env env counts pn |> fun (cpn, e, c) -> cpn :: cpp, e, c)
        ([], env3, counts3)
        pp
    in
    CPTuple (cp1, cp2, List.rev cpp), env4, counts4
;;

(* The top-level function *)
let rec cps_glob ds_ref_once ds_no_refs glob_env =
  let one_ref i =
    match ISet.find_opt i.id ds_ref_once with
    | None -> false
    | Some _ -> true
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
      cps env' e1 (LetRecCont (pat, e2, env', c)) counts'
    | DETuple (e1, e2, ee) -> cps env e1 (TupleBldCont (e2 :: ee, [], env, c)) counts
  (* Three smart constructors, for RET, CALL & IF forms. *)
  and ret c a counts =
    let helper_toplevelletcont = function
      | (NonRecursive, y', e) :: tl -> ToplevelLetNonRecCont (y', tl), e, glob_env, counts
      | [] -> AHALT, DEUnit, glob_env, counts
      | (Recursive, y', e) :: tl ->
        let pat, glob_env', counts' = extend_env glob_env counts y' in
        ToplevelLetRecCont (pat, tl), e, glob_env', counts'
    in
    let bless_c_and_a c a =
      let cont, counts2 = blessc c counts in
      let arg, counts3 = blessa a counts2 in
      cont, arg, counts3
    in
    match c with
    | AHALT | KVar _ ->
      let cont, arg, counts2 = bless_c_and_a c a in
      Ret (cont, Triv arg), counts2
    | FCont (e, env, c') -> cps env e (ACont (a, c')) counts
    | ACont (a', c') -> call a' a c' counts
    | ICont (e1, e2, env, c') -> cif a e1 e2 c' env counts
    | BinopsFirstArgCont (op, e, env, c') ->
      let arg1, counts2 = blessa a counts in
      cps env e (BinopsSecondArgCont (op, arg1, c')) counts2
    | BinopsSecondArgCont (op, arg1, c') ->
      let arg2, counts2 = blessa a counts in
      ret c' (APrimBinop (op, arg1, arg2)) counts2
    | TupleBldCont (ds_ee, trivs, env, c') ->
      let arg, counts2 = blessa a counts in
      (match ds_ee with
       | [] -> ret c' (atuple (arg :: trivs)) counts2
       | hd :: tl -> cps env hd (TupleBldCont (tl, arg :: trivs, env, c')) counts2)
    | LetNonRecCont (y, wh, env, c') ->
      bnd cps y a wh env c' (fun x b w -> Let (NonRecursive, x, b, w)) counts
    | LetRecCont (pat, wh, env, c') ->
      bnd_rec cps a wh env c' (fun b w -> Let (Recursive, pat, b, w)) counts
    | ToplevelLetNonRecCont (y, vbs) ->
      let constr x b w = Let (NonRecursive, x, b, w) in
      let c'', e, glob_env', counts' = helper_toplevelletcont vbs in
      bnd (cps_glob ds_ref_once ds_no_refs) y a e glob_env' c'' constr counts'
    | ToplevelLetRecCont (pat, vbs) ->
      let c'', e, glob_env', counts' = helper_toplevelletcont vbs in
      let constr b w = Let (NonRecursive, pat, b, w) in
      bnd_rec (cps_glob ds_ref_once ds_no_refs) a e glob_env' c'' constr counts'
  and call f a c counts =
    match f with
    | AVar v when String.equal v.hum_name "print" ->
      let arg, counts2 = blessa a counts in
      let cont, counts3 = blessc c counts2 in
      Ret (cont, Print arg), counts3
    | AVar _ | AConst _ | AUnit | APrimBinop _ | ATuple _ ->
      let func, counts2 = blessa f counts in
      let arg, counts3 = blessa a counts2 in
      let cont, counts4 = blessc c counts3 in
      Call (func, arg, cont), counts4
    | AClo (y, body, env) ->
      bnd cps y a body env c (fun x arg b -> Ret (Cont (x, b), Triv arg)) counts
  and cif a e1 e2 c env counts =
    match c with
    (* unit size conts*)
    | AHALT | KVar _ ->
      let test, counts2 = blessa a counts in
      let conseq, counts3 = cps env e1 c counts2 in
      let alt, counts4 = cps env e2 c counts3 in
      CIf (test, conseq, alt), counts4
    (* other conts*)
    | _ ->
      let jv = gensym ~prefix:"jv" () |> of_string in
      let body, counts2 = cif a e1 e2 (KVar jv) env counts in
      let join, counts3 = blessc c counts2 in
      Letc (jv, join, body), counts3
  and bnd cps_func y a wh env c constr counts =
    match y with
    | DPTuple _ ->
      let b, counts2 = blessa a counts in
      let pat, env', counts3 = extend_env env counts2 y in
      let w, counts4 = cps_func env' wh c counts3 in
      constr pat b w, counts4
    | DPVar i ->
      if one_ref i
      then cps_func (extend i.id a env) wh c counts
      else (
        let b, counts2 = blessa a counts in
        match b, Option.is_some (ISet.find_opt i.id ds_no_refs) with
        | TUnit, false -> cps_func (extend i.id AUnit env) wh c counts2
        | UVar x, false -> cps_func (extend i.id (AVar x) env) wh c counts2
        | TConst z, false -> cps_func (extend i.id (AConst z) env) wh c counts2
        | _ ->
          let pat, env', counts3 = extend_env env counts2 y in
          let w, counts4 = cps_func env' wh c counts3 in
          constr pat b w, counts4)
  and bnd_rec cps_func a wh env c constr counts =
    let b, counts2 = blessa a counts in
    let w, counts3 = cps_func env wh c counts2 in
    constr b w, counts3
  (* Two "blessing" functions to render abstract continuations
     and abstract arguments into actual syntax. *)
  and blessc c counts =
    match c with
    | AHALT -> HALT, counts
    | KVar kv -> CVar kv, counts
    | _ ->
      let x = gensym ~prefix:"t" () |> of_string in
      let counts2 = new_count x.id counts in
      let body, counts3 = ret c (AVar x) counts2 in
      Cont (CPVar x, body), counts3
  and blessa a counts =
    match a with
    | AUnit -> TUnit, counts
    | AVar x -> UVar x, incr x.id counts
    | AConst z -> TConst z, counts
    | ATuple t -> TTuple t, counts
    | APrimBinop pb -> PrimBinop pb, counts
    | AClo (y, body, env) ->
      let pat, env', counts' = extend_env env counts y in
      let k = gensym ~prefix:"k" () |> of_string in
      let b, counts'' = cps env' body (KVar k) counts' in
      (* The eta-reduction check. Note that we don't have to check
         reference counts on k, as continuation variables are linear. *)
      (match b, pat with
       | Call (f, UVar x', CVar k'), CPVar x ->
         if x = x' && k = k' && IMap.find x.id counts'' = 1
         then f, counts'
         else Lam (pat, k, b), counts'
       | _ -> Lam (pat, k, b), counts')
  in
  cps glob_env
;;

let cps_vb ds_ref_once ds_no_refs (rec_flag, ds_pat, ds_expr) =
  let open IMap in
  let pat, glob_env, counts = extend_env (fst start_glob_envs) empty ds_pat in
  let p, _ =
    match rec_flag with
    | Recursive -> cps_glob ds_ref_once ds_no_refs glob_env ds_expr AHALT counts
    | NonRecursive ->
      cps_glob ds_ref_once ds_no_refs (fst start_glob_envs) ds_expr AHALT empty
  in
  rec_flag, pat, p
;;

let cps_program ds_ref_once ds_no_refs vbs =
  let open IMap in
  let open Miniml.Ident in
  let p, _ =
    match vbs with
    | [] -> Ret (HALT, Triv TUnit), empty
    | (NonRecursive, ds_pat, ds_expr) :: tl ->
      cps_glob
        ds_ref_once
        ds_no_refs
        (fst start_glob_envs)
        ds_expr
        (ToplevelLetNonRecCont (ds_pat, tl))
        empty
    | (Recursive, ds_pat, ds_expr) :: tl ->
      let pat, glob_env, counts = extend_env (fst start_glob_envs) empty ds_pat in
      cps_glob
        ds_ref_once
        ds_no_refs
        glob_env
        ds_expr
        (ToplevelLetRecCont (pat, tl))
        counts
  in
  NonRecursive, CPVar (of_string "cps_program"), p
;;

let cps_vb_to_parsetree_vb (rec_flag, pat, p) =
  let helper_ptuple dp1 dp2 dps k' = k' (PTuple (dp1, dp2, dps)) in
  let rec helper_list f k' = function
    | [] -> k' []
    | hd :: tl -> (fun el -> helper_list f (fun els -> k' (el :: els)) tl) |> f hd
  in
  let rec helper_pat pat k' =
    match pat with
    | CPVar i -> k' (pvar i.hum_name)
    | CPTuple (pat1, pat2, pats) ->
      (fun dp1 ->
        helper_pat pat2 (fun dp2 ->
          helper_list helper_pat (fun dps -> helper_ptuple dp1 dp2 dps k') pats))
      |> helper_pat pat1
  in
  let helper_econst z k' = k' (EConst z) in
  let helper_etuple e1 e2 ee k' = k' (ETuple (e1, e2, ee)) in
  let rec helper_triv t k' =
    match t with
    | TUnit -> EUnit |> k'
    | TTuple (t1, t2, tt) ->
      (fun e1 ->
        helper_triv t2 (fun e2 ->
          helper_list helper_triv (fun ee -> helper_etuple e1 e2 ee k') tt))
      |> helper_triv t1
    | UVar i -> k' (evar i.hum_name)
    | TConst d -> helper_econst d k'
    | Lam (pat, i, p) ->
      (fun ptrn -> helper_p p (fun b -> elam ptrn (elam (pvar i.hum_name) b) |> k'))
      |> helper_pat pat
    (* | Print t' -> helper_triv t' (fun e -> EApp (EVar "print", e) |> k') *)
    | PrimBinop (op, t1, t2) ->
      helper_triv t1 (fun e1 ->
        helper_triv t2 (fun e2 -> eapp (evar op.hum_name) [ e1; e2 ] |> k'))
  and helper_cont cont k' =
    match cont with
    | Cont (pat, p) ->
      (fun ptrn -> helper_p p (fun e -> elam ptrn e |> k')) |> helper_pat pat
    | CVar v -> k' (evar v.hum_name)
    | HALT -> ELam (PVar "x", EVar "x") |> k'
  and helper_p p k =
    match p with
    | Call (t1, t2, c) ->
      (fun e1 ->
        helper_triv t2 (fun e2 -> helper_cont c (fun e3 -> eapp e1 [ e2; e3 ] |> k)))
      |> helper_triv t1
    | Ret (HALT, Triv t) -> helper_triv t k
    | Ret (HALT, Print t) -> (fun t -> eapp1 (evar "print") t |> k) |> helper_triv t
    | Ret (c, Triv t) ->
      (fun e1 -> helper_triv t (fun e2 -> eapp1 e1 e2 |> k)) |> helper_cont c
    | Ret (c, Print t) ->
      (fun e -> helper_triv t (fun arg -> eapp1 e (eapp1 (evar "print") arg) |> k))
      |> helper_cont c
    | CIf (t, p1, p2) ->
      (fun e1 -> helper_p p1 (fun e2 -> helper_p p2 (fun e3 -> eite e1 e2 e3 |> k)))
      |> helper_triv t
    | Letc (v, c, p) ->
      helper_cont c (fun e1 -> helper_p p (fun e2 -> elet (pvar v.hum_name) e1 e2 |> k))
    | Let (rec_flag, pat, t, p) ->
      (fun ptrn ->
        helper_triv t (fun e1 ->
          helper_p p (fun e2 -> elet ~isrec:rec_flag ptrn e1 e2 |> k)))
      |> helper_pat pat
  in
  (fun ptrn -> helper_p p (fun e -> rec_flag, ptrn, e)) |> helper_pat pat
;;

let error_message free_vars =
  String.concat
    "\n"
    ("Variables are not in scope:" :: SMap.fold (fun v _ acc -> v :: acc) free_vars [])
;;

let test_cps_program text =
  let open Miniml in
  match Parsing.parse_structure text with
  | Result.Ok stru ->
    (match distr_ids_program stru with
     | Ok ds_stru ->
       let _, ref_once, no_refs = preconv_count_program ds_stru in
       let cps_program = cps_program ref_once no_refs ds_stru in
       let vb' = cps_vb_to_parsetree_vb cps_program in
       Format.printf "%a" Pprint.pp_value_binding vb';
       ANF.reset_gensym ()
     | Error free_vars -> print_endline (error_message free_vars))
  | _ -> Format.printf "parsing error\n"
;;

let test_cps_vb text =
  let open Miniml in
  let vb = Parsing.parse_vb_exn text in
  match distr_ids_vb vb with
  | Error free_vars -> print_endline (error_message free_vars)
  | Ok ds_vb ->
    let _, ref_once, no_refs = preconv_count_vb ds_vb in
    let cps_vb = cps_vb ref_once no_refs ds_vb in
    let vb' = cps_vb_to_parsetree_vb cps_vb in
    Format.printf "%a" Pprint.pp_value_binding vb';
    ANF.reset_gensym ()
;;

let%expect_test "cps simple func" =
  test_cps_vb {| let double x = 2 * x|};
  [%expect {| let double x k1 = k1 (2 * x)
|}]
;;

let%expect_test "cps simple prog" =
  test_cps_program {| let double x = 2 * x 
  let main = double (double 3)|};
  [%expect
    {| 
  let cps_program = let double x k1 = k1 (2 * x) in double 3 (fun t2 ->
                                                              double t2
                                                              (fun t3 ->
                                                               let main =
                                                               t3 in ()))
|}]
;;

let%expect_test "cps prog inlining" =
  test_cps_program {| let y = 3 
  let double x = 2 * x 
  let main = double (y + y)|};
  [%expect {|  let cps_program = let main = 2 * (3 + 3) in ()
|}]
;;

let%expect_test "cps rec func (inlining banned)" =
  test_cps_vb {| let y = let rec t x = t 1 in t 2|};
  [%expect {| let y = let rec t x k1 = t 1 k1 in t 2 (fun x -> x)
|}]
;;

let%expect_test "cps eta" =
  test_cps_vb {| 
   let main = let g x = x in (fun x -> g x) g|};
  [%expect {| let main = let g x k1 = k1 x in g g (fun x -> x)
|}]
;;

let%expect_test "cps eta let" =
  test_cps_vb {| 
   let main = let g x = x in let f y = g y in f (g 0)|};
  [%expect {| let main = let g x k1 = k1 x in g 0 (fun t2 -> g t2 (fun x -> x))
|}]
;;

let%expect_test "cps fac" =
  test_cps_vb {| let rec fac n = if n = 1 then 1 else fac (n-1) * n|};
  [%expect
    {| let rec fac n k1 = if n = 1 then k1 1 else fac (n - 1) (fun t2 -> k1 (t2 * n))
|}]
;;

let%expect_test "cps complex branching" =
  test_cps_vb {| let x f = 1 + if (f 2) then 3 else 5|};
  [%expect
    {| 
    let x f k1 = f 2 (fun t2 -> let jv3 t4 = k1 (1 + t4) in if t2 then jv3 3
                                                            else jv3 5)
|}]
;;

let%expect_test "cps double print" =
  test_cps_vb {| let f g = print (print 1)|};
  [%expect {| let f g k1 = (fun t2 -> k1 (print t2)) (print 1)
|}]
;;

let%expect_test "cps arg-print " =
  test_cps_vb {| let f = (fun x -> x) (print 1)|};
  [%expect {| let f = (fun t1 -> t1) (print 1)
|}]
;;

let%expect_test "cps print alias " =
  test_cps_vb {| let f = let p = print in p 0|};
  [%expect {| let f = print 0
|}]
;;

let%expect_test "cps one ref arg-binop" =
  test_cps_vb {| let f = let g x = x + 1 in g (2 * 2)   |};
  [%expect {| let f = (2 * 2) + 1 
|}]
;;

(* или для результата арифм опреции всегда стоит вводить новую переменную?*)
let%expect_test "cps one ref binop" =
  test_cps_vb {| let f g = let x = 2 * 2 in g x |};
  [%expect {| let f g k1 = g (2 * 2) k1 
|}]
;;

let%expect_test "cps mult refs arg-const" =
  test_cps_vb {| let f g = let x = 2 in g x x|};
  [%expect {| let f g k1 = g 2 (fun t2 -> t2 2 k1)
|}]
;;

let%expect_test "cps  mult refs arg-binop" =
  test_cps_vb {| let f g = let x = 2 * 2 in g x x|};
  [%expect {| let f g k1 = let x = 2 * 2 in g x (fun t2 -> t2 x k1) 
|}]
;;

let%expect_test "cps complex tuple-arg" =
  test_cps_vb {| let f g = g (g 3, 1)|};
  [%expect {| let f g k1 = g 3 (fun t2 -> g (t2, 1) k1) 
|}]
;;

let%expect_test "cps ptuple" =
  test_cps_vb {| let f (x,y) = x + y|};
  [%expect {| let f (x, y) k1 = k1 (x + y)
|}]
;;

let%expect_test "cps free vars" =
  test_cps_vb {| let f x = x + y + z|};
  [%expect {| 
  Variables are not in scope:
  z
  y
|}]
;;

let%expect_test "cps func in func" =
  test_cps_vb {| let z = let f = fun x -> print in f 2 3|};
  [%expect {|  let z = (fun x -> print 3) 2
|}]
;;
