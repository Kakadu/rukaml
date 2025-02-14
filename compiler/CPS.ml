(* https://www.khoury.northeastern.edu/home/shivers/papers/nobrainer-cps.pdf *)

open Miniml.Parsetree
open Miniml.Ident

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

let ( let+ ) = Base.Result.( >>| )
let ( let* ) = Base.Result.( >>= )

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
  | Ret of cont * triv
  | CIf of triv * p * p
  | Let of rec_flag * pat * triv * p
  | Primop of pat * var * triv list * p
  | Letc of var * cont * p

and cont =
  | Cont of pat * p
  | CVar of var
  | HALT

and 'a tuple = 'a * 'a * 'a list
and 'a safe_binop = var * 'a * 'a (* invarint:  division by zero isn't possible *)

and triv =
  | Lam of pat * var * p
  | TSafeBinop of triv safe_binop
  | UVar of var
  | TConst of const
  | TTuple of triv tuple
  | TUnit

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
  | LetRecCont of pat * ds_expr * env * c * ds_expr option
  | ToplevelLetRecCont of pat * ds_vb list * main_id * ds_expr option
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
      let from_vars = SMap.find_opt hum_name vars in
      let builtins = snd start_glob_envs in
      let from_builtins = SMap.find_opt hum_name builtins in
      (match from_builtins with
       | Some id -> k (DEVar { hum_name; id }) free_vars
       | None ->
         (match from_vars with
          | None ->
            let from_free_vars = SMap.find_opt hum_name free_vars in
            (match from_free_vars with
             | None ->
               let ident = of_string hum_name in
               let k = k (DEVar { hum_name; id = ident.id }) in
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
  let vb = Miniml.Parsing.parse_vb_exn text in
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
      cps env' e1 (LetRecCont (pat, e2, env', c, maybe_not_allowed_expr e1)) counts'
    | DETuple (e1, e2, ee) -> cps env e1 (TupleBldCont (e2 :: ee, [], env, c)) counts
  (* Three smart constructors, for RET, CALL & IF forms. *)
  and ret c a counts =
    let helper_toplevelletcont main_id = function
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
      let c'', e, glob_env', counts2 = helper_toplevelletcont main_id vbs in
      bnd cps_glob y a e glob_env' c'' constr counts2
    | ToplevelLetRecCont (pat, vbs, main_id, dang_expr) ->
      let c'', e, glob_env', counts2 = helper_toplevelletcont main_id vbs in
      let constr b w = Let (Recursive, pat, b, w) in
      bnd_rec cps_glob a e glob_env' c'' constr counts2 dang_expr pat
    | BinopsFirstArgCont (op, e, env, c') ->
      cps env e (BinopsSecondArgCont (op, a, c')) counts
    | BinopsSecondArgCont (op, a1, c') -> binop op a1 a c' counts
  and call f a c counts =
    match f with
    | AVar v when String.equal v.hum_name "print" -> primop v [ a ] c counts
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
        (match b, Option.is_some (ISet.find_opt i.id ds_no_refs) with
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
    | _, (AConst (PConst_int 0) | AVar _), "/" -> primop op [ a1; a2 ] c counts
    | _ -> ret c (ASafeBinop (op, a1, a2)) counts
  and primop f aa c counts =
    let* counts2, args = blessa_many counts aa in
    let x = gensym ~prefix:"x" () |> of_string in
    let counts3 = new_count x.id counts2 in
    let+ wh, counts4 = ret c (AVar x) counts3 in
    Primop (CPVar x, f, args, wh), counts4
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
  let open Miniml.Ident in
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

let cps_vb_to_parsetree_vb (rec_flag, pat, p) =
  let rec helper_pat pat k' =
    match pat with
    | CPVar i -> k' (PVar i.hum_name)
    | CPTuple (pat1, pat2, pats) ->
      let k ptrn1 ptrn2 ptrns = k' (PTuple (ptrn1, ptrn2, ptrns)) in
      tuple_fold_map_k helper_pat pat1 pat2 pats k
  in
  let rec helper_triv t k' =
    match t with
    | TUnit -> k' EUnit
    | TTuple (t1, t2, tt) ->
      tuple_fold_map_k helper_triv t1 t2 tt (fun e1 e2 ee -> k' (ETuple (e1, e2, ee)))
    | UVar i -> k' (EVar i.hum_name)
    | TConst d -> k' (EConst d)
    | Lam (pat, i, p) ->
      let k1 ptrn =
        helper_p p (fun b ->
          let l1 = elam (pvar i.hum_name) b in
          let l2 = elam ptrn l1 in
          k' l2)
      in
      helper_pat pat k1
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
    match p with
    | Call (t1, t2, c) ->
      helper_triv t1 (fun e1 ->
        helper_triv t2 (fun e2 ->
          helper_cont c (fun e3 ->
            let res = eapp e1 [ e2; e3 ] in
            k res)))
    | Ret (HALT, t) -> helper_triv t k
    | Ret (c, t) -> helper_cont c (fun e1 -> helper_triv t (fun e2 -> k (EApp (e1, e2))))
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
        helper_triv t (fun e1 -> helper_p p (fun e2 -> k (ELet (rec_flag, ptrn, e1, e2)))))
    | Primop (pat, f, tt, p) ->
      (fun ptrn ->
        list_fold_map_k helper_triv tt (fun ee ->
          helper_p p (fun e2 ->
            let body = eapp (EVar f.hum_name) ee in
            k (ELet (rec_flag, ptrn, body, e2)))))
      |> helper_pat pat
  in
  helper_pat pat (fun ptrn -> helper_p p (fun e -> rec_flag, ptrn, e))
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
      Miniml.Pprint.pp_expr
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
  let open Miniml in
  let stru = Result.get_ok @@ Parsing.parse_structure text in
  match cps_conv_program stru with
  | Ok cps_program ->
    let vb' = cps_vb_to_parsetree_vb cps_program in
    Format.printf "%a" Pprint.pp_value_binding vb';
    ANF.reset_gensym ()
  | Error e -> Format.printf "%a\n%!" pp_error e
;;

let test_cps_vb text =
  let open Miniml in
  match cps_conv_vb @@ Parsing.parse_vb_exn text with
  | Error e -> Format.printf "%a\n%!" pp_error e
  | Ok cps_vb ->
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
let main = let double x k1 = k1 (2 * x) in double 3 (fun t2 -> double
                                                               t2 (fun
                                                                  t3 ->
                                                                   t3))
|}]
;;

let%expect_test "cps prog inlining" =
  test_cps_program {|let y = 3 
  let double x = 2 * x 
  let main = double (y + y)|};
  [%expect {|  let main = 2 * (3 + 3)
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

let%expect_test "cps print" =
  test_cps_vb {|let main  = (fun z -> 1) (print 0) |};
  [%expect {|   let main = let x1 = print 0 in (fun z -> 1) x1
|}]
;;

let%expect_test "cps print alias " =
  test_cps_vb {| let f = let p = print in let z = p 0 in z + 1|};
  [%expect {| let f = let x1 = print 0 in x1 + 1
|}]
;;

let%expect_test "cps one ref arg-binop" =
  test_cps_vb {| let f = let g x = x + 1 in g (2 * 2)   |};
  [%expect {| let f = (2 * 2) + 1 
|}]
;;

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

let%expect_test "cps  mult refs arg-binop (inlining banned)" =
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
  test_cps_vb {| let z = let rec g y = y in  let f = fun x -> g in f 2 3|};
  [%expect {|  let z = let rec g y k1 = k1 y in (fun x -> g 3 (fun x -> x)) 2
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
  [%expect {|  let main = (fun y -> let rec x = 8 in 0) 12
|}]
;;
