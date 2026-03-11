(* https://www.khoury.northeastern.edu/home/shivers/papers/nobrainer-cps.pdf *)

open Frontend
open Frontend.Parsetree
open Frontend.Ident

(* ds_pattern, ds_expr, ds_vb --- that's parsetree but every var has id *)
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
  | DEArray of ds_expr list
  | DELet of rec_flag * ds_pattern * ds_expr * ds_expr

let ( >>| ) = Base.Result.( >>| )
let ( let+ ) = ( >>| )
let ( let* ) = Base.Result.( >>= )

module SMap = Map.Make (String)

type 'a string_map = 'a SMap.t

module IMap = Map.Make (Int)
module ISet = Set.Make (Int)

let gensym = ANF.gensym_s
let tuple_fold_map_k = CPSLang.tuple_fold_map_k
let list_fold_map_k = CPSLang.list_fold_map_k

open CPSLang.OneACPS

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

(* Abstract continuations *)
type c =
  | AHALT
  | KVar of var
  | FCont of ds_expr * env * c
  | ACont of a * c
  | ICont of ds_expr * ds_expr * env * c
  | TupleBldCont of ds_expr list * a list * env * c
  (* | ArrayBldCont of ds_expr list * a list * env * c *)
  | LetRecCont of pat * ds_expr * env * c * potent_not_allowed_expr
  | LetNonRecCont of ds_pattern * ds_expr * env * c
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
let incr x counts = IMap.update x (Option.map @@ ( + ) 1) counts

let start_glob_envs =
  let extend v =
    let ident = Frontend.Ident.of_string v in
    SMap.add ident.hum_name ident.id
  in
  let idents = [ "print"; "closure_count"; "length"; "get"; "set" ] in
  let idents = List.map (fun x -> Frontend.Ident.of_string x) idents in
  let imap =
    List.fold_right
      (fun x acc -> IMap.add x.id (AVar x) acc)
      (List.tl idents)
      IMap.(add (List.hd idents).id (AVar (List.hd idents)) empty)
  in
  let smap =
    List.fold_right (fun x acc -> SMap.add x.hum_name x.id acc) idents SMap.empty
  in
  ( imap
  , SMap.(
      (* empty *)
      (* |> add printi.hum_name printi.id *)
      (* |> add closure_count.hum_name closure_count.id) *)
      smap
      |> extend "<"
      |> extend ">"
      |> extend "<="
      |> extend ">="
      |> extend "="
      |> extend "+"
      |> extend "-"
      |> extend "*"
      |> extend "/"
      |> extend "&&"
      |> extend "||") )
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
    | _ -> failwith "not implemented"
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
    | EArray l ->
      (* TODO(nikita): Test this*)
      let k l = k (DEArray l) in
      let helper_e e k = helper_e e vars k in
      list_fold_map_k helper_e l k free_vars
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
    | EMatch _ | EConstruct _ -> failwith "not implemented"
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
    var x got id 65
    var y got id 66
    var z got id 67
    var m got id 68
    id: 65; counts 1
    id: 66; counts 2
    id: 67; counts 0
    id: 68; counts 0
    ids that ref_once:
    65
    ids that no_refs:
    67
    68
    |}]
;;

let%expect_test "counts branching, shadowing" =
  test_count {| let m x y = if x then fun x -> x 1 else fun x -> (y , y x)|};
  [%expect
    {|
    var x got id 69
    var y got id 70
    var x got id 71
    var x got id 72
    var m got id 73
    id: 69; counts 1
    id: 70; counts 2
    id: 71; counts 1
    id: 72; counts 1
    id: 73; counts 0
    ids that ref_once:
    69
    71
    72
    ids that no_refs:
    73
    |}]
;;

let%expect_test "counts rec, ptuple" =
  test_count {| let rec (x,y) = x x y|};
  [%expect
    {|
    var x got id 74
    var y got id 75
    id: 74; counts 2
    id: 75; counts 1
    ids that ref_once:
    75
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

let decide_if_not_alowed counts pat = function
  | Some e when has_nonzero_counts_vars counts pat -> Error (`Let_rec_not_allowed e)
  | _ -> Ok ()
;;

(* The top-level function *)
let of_vb ds_ref_once ds_no_refs glob_env =
  let open Base.Result in
  let one_ref i = ISet.mem i.id ds_ref_once in
  let rec cps env exp c counts =
    match exp with
    | DEVar y -> ret c (IMap.find y.id env) counts
    | DEArray _ -> failwith "unimplemented"
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
    | BinopsFirstArgCont (op, e, env, c') ->
      cps env e (BinopsSecondArgCont (op, a, c')) counts
    | BinopsSecondArgCont (op, a1, c') -> binop op a1 a c' counts
  and call f a c counts =
    match f with
    | AVar v when String.equal v.hum_name "print" -> primop v a [] c counts
    | AVar v when String.equal v.hum_name "closure_count" -> primop v a [] c counts
    | AVar _ | AConst _ | AUnit | ASafeBinop _ | ATuple _ ->
      (match a with
       | AVar v when String.equal v.hum_name "print" ->
         let x = gensym ~prefix:"x" () |> of_string in
         let k = gensym ~prefix:"k" () |> of_string in
         let* func, counts2 = blessa f counts in
         let* cont, counts3 = blessc c counts2 in
         let+ b = primop v (AVar x) [] (KVar k) counts in
         Call (func, Lam (CPVar x, k, fst b), cont), counts3
       | _ ->
         let* func, arg, counts2 = blessa2 f a counts in
         let+ cont, counts3 = blessc c counts2 in
         Call (func, arg, cont), counts3)
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
    let* () = decide_if_not_alowed counts2 pat dang_expr in
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
    | DEArray l ->
      (* TODO(nikita): Test this*)
      list_fold_map_k helper l (fun l -> k (EArray l))
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

let cps_conv vbs =
  let open IMap in
  let k ds_vb _ free_vars _ no_refs ref_once =
    let k1 () = Ok (ds_vb, no_refs, ref_once) in
    free_vars_check k1 free_vars
  in
  let* ds_vbs, ds_no_refs, ds_ref_once =
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
  Result.map (fun (_, cps_vbs) -> List.rev cps_vbs)
  @@ Base.List.fold_result
       ds_vbs
       ~init:(fst start_glob_envs, [])
       ~f:(fun (glob_env, cps_vbs_acc) (rec_flag, ds_pat, ds_expr) ->
         let pat, glob_env2, counts2 = extend_env glob_env empty ds_pat in
         let+ p, _ =
           match rec_flag with
           | Recursive ->
             let* ((_, counts3) as res) =
               of_vb ds_ref_once ds_no_refs glob_env2 ds_expr AHALT counts2
             in
             Result.map (Fun.const res)
             @@ decide_if_not_alowed counts3 pat
             @@ maybe_not_allowed_expr ds_expr
           | NonRecursive -> of_vb ds_ref_once ds_no_refs glob_env ds_expr AHALT empty
         in
         glob_env2, (rec_flag, pat, p) :: cps_vbs_acc)
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

let test_cps text =
  let open Frontend in
  let stru = Result.get_ok @@ Parsing.parse_value_bindings text in
  match cps_conv stru with
  | Ok cps_prog ->
    Format.printf "%a" pp_stru cps_prog;
    ANF.reset_gensym ()
  | Error e -> Format.printf "%a\n%!" pp_error e
;;

let test_cps text =
  let open Frontend in
  let stru = Result.get_ok @@ Parsing.parse_value_bindings text in
  match cps_conv stru with
  | Ok cps_prog ->
    Format.printf "%a" pp_stru cps_prog;
    ANF.reset_gensym ()
  | Error e -> Format.printf "%a\n%!" pp_error e
;;
