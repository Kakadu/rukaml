open Frontend
open Frontend.Ident
open Frontend.Parsetree
open CPSLang
open CPSLang.OneACPS
module IMap = Map.Make (Int)
module ISet = Set.Make (Int)

let gensym = ANF.gensym_s

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

  type bnd_unsafety =
    (*`SideEffUnsafe < Unsafe *)
    | Unsafe
    | SideEffUnsafe
  [@@deriving show { with_path = false }]

  let anal (_, _, p) bnd_unsafities =
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
    let take_safety_into_acc unsafety v_id b_co_calls v_arity =
      match unsafety with
      | Some SideEffUnsafe -> 0
      | Some Unsafe when has_loop v_id b_co_calls -> 0
      | _ -> v_arity
    in
    let rec anal_p conts ress inc_ar p int =
      let open IMap in
      let anal_tt0_ign a aa ress2 = anal_tt0 conts int a aa ress2 |> ignore_fst in
      let anal_triv0_ign t ress = anal_triv conts int 0 t ress |> ignore_fst in
      let lam_call_anal0_ign lam_b pat a ress =
        ignore_fst @@ lam_call_anal lam_b conts int 0 ress pat a
      in
      let clear_lam_call_anal lam_b pat a =
        lam_call_anal lam_b conts int inc_ar ress pat a
      in
      let clear_triv_ret_anal t = anal_triv conts int inc_ar t ress in
      match p with
      | CIf (c, th, el) -> anal_cif c th el conts int ress inc_ar
      | Call (Lam (pat, c, lam_b), a, Cont (CPVar { id; _ }, body)) ->
        let had_upd, unsafety = false, find_opt id bnd_unsafities in
        fin_lam_call_bnd_anal ~had_upd c lam_b conts int pat a unsafety
        @@ anal_bnd1 id conts body int ress inc_ar
      | Call (Lam (pat, _, lam_b), a, Cont (CPTuple _, body)) ->
        let rhs_anal = lam_call_anal0_ign lam_b pat a in
        fin_unint_bnd_anal rhs_anal @@ anal_p conts ress inc_ar body int
      | Call (Lam (pat, c, lam_b), a, CVar { id; _ }) ->
        (match find id conts with
         | `Int fin_anal ->
           let had_upd = true in
           let unsafety = find_opt id bnd_unsafities in
           (ress, fin_anal)
           |> fin_lam_call_bnd_anal ~had_upd c lam_b conts int pat a unsafety
         | `UnInt (b_co_calls, b_ars) ->
           fin_unint_bnd_anal (lam_call_anal0_ign lam_b pat a) (b_co_calls, b_ars, ress)
         | exception Not_found -> clear_lam_call_anal lam_b pat a)
      | Call (Lam (pat, _, lam_b), a, HALT) -> clear_lam_call_anal lam_b pat a
      | Call (f, a, Cont (CPVar { id; _ }, body)) ->
        let unsafety = find_opt id bnd_unsafities in
        fin_bnd_call_anal unsafety conts int f a
        @@ anal_bnd1 id conts body int ress inc_ar
      | Call (f, a, CVar { id; _ }) ->
        (match find id conts with
         | `Int fin_anal ->
           let unsafety = find_opt id bnd_unsafities in
           fin_bnd_call_anal unsafety conts int f a (ress, fin_anal)
         | `UnInt (b_co_calls, b_ars) ->
           (b_co_calls, b_ars, ress) |> fin_unint_bnd_anal @@ anal_tt0_ign f [ a ]
         | exception Not_found -> anal_tt0 ~fst_inc_ar:(inc_ar + 1) conts int f [ a ] ress)
      | Call (f, a, HALT) -> anal_tt0 ~fst_inc_ar:(inc_ar + 1) conts int f [ a ] ress
      | Ret (CVar { id; _ }, t) ->
        (match find id conts with
         | `Int fin_anal ->
           fin_int_triv_bnd_anal ~had_upd:true conts int t (ress, fin_anal)
         | `UnInt (b_co_calls, b_ars) ->
           fin_unint_bnd_anal (anal_triv0_ign t) (b_co_calls, b_ars, ress)
         | exception Not_found -> clear_triv_ret_anal t)
      | Ret (Cont (CPVar { id; _ }, body), t)
      | Let (NonRecursive, CPVar { id; _ }, t, body)
        when is_int_triv_rhs int t ->
        fin_int_triv_bnd_anal conts int t @@ anal_bnd1 id conts body int ress inc_ar
      | Ret (Cont ((CPTuple _ | CPVar _), body), t)
      | Let (NonRecursive, CPVar _, t, body)
      | Let (_, CPTuple _, t, body) ->
        fin_unint_bnd_anal (anal_triv0_ign t) @@ anal_p conts ress inc_ar body int
      | Primop (_, _, t, tt, body) ->
        fin_unint_bnd_anal (anal_tt0_ign t tt) @@ anal_p conts ress inc_ar body int
      | Call (f, a, Cont (CPTuple _, body)) ->
        fin_unint_bnd_anal (anal_tt0_ign f [ a ]) @@ anal_p conts ress inc_ar body int
      | Letc ({ id = jv_id; _ }, Cont (CPVar { id; _ }, body), p) ->
        let jv_specif = Some jv_id in
        let ress2, fin_anal = anal_bnd1 ~jv_specif id conts body int ress inc_ar in
        let conts2 = add jv_id (`Int fin_anal) conts in
        anal_p conts2 ress2 0 p int
      | Letc ({ id = jp_id; _ }, Cont (CPTuple _, body), p) ->
        let b_co_calls, b_ars, ress2 = anal_p conts ress inc_ar body int in
        let conts2 = add jp_id (`UnInt (b_co_calls, b_ars)) conts in
        anal_p conts2 ress2 0 p int
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
        anal_p conts2 ress2 inc_ar p int
      | Ret (HALT, t) -> clear_triv_ret_anal t
      | Letc (_, HALT, p) -> anal_p conts ress inc_ar p int
      | Let (Recursive, CPVar { id; _ }, t, body) ->
        let int2 = ISet.add id int in
        anal_rec_bnd id conts int2 t @@ anal_p conts ress inc_ar body int2
    and anal_bnd1 ?(jv_specif = None) id conts body int ress inc_ar =
      anal_bnd_cont ~jv_specif id @@ anal_p conts ress inc_ar body @@ ISet.add id int
    and anal_bnd_cont ?(jv_specif = None) v_id (b_co_calls, b_ars, ress) =
      let default v_arity =
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
      in
      match IMap.find v_id b_ars with
      | exception Not_found ->
        (* dead var case *)
        (match IMap.find v_id bnd_unsafities with
         | SideEffUnsafe -> default 0
         | (exception Not_found) | Unsafe ->
           let dead_id = Option.value jv_specif ~default:v_id in
           ( { ress with dead_vars = ISet.add dead_id ress.dead_vars }
           , fun ress2 _ -> b_co_calls, b_ars, ress2 ))
      | v_arity -> default v_arity
    and anal_rec_bnd v_id conts int t (b_co_calls, b_ars, ress) =
      let default v_arity =
        let rec fixpointing v_ar has_loop_v =
          let (rhs_co_calls, rhs_ars, ress2), rhs_fv =
            anal_triv conts int v_ar t ress
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
      in
      match IMap.find v_id b_ars with
      | exception Not_found ->
        (* dead var case *)
        (match IMap.find v_id bnd_unsafities with
         | SideEffUnsafe -> default 0
         | (exception Not_found) | Unsafe ->
           b_co_calls, b_ars, { ress with dead_vars = ISet.add v_id ress.dead_vars })
      | v_arity -> default v_arity
    and fin_bnd_call_anal unsafety conts int f a (ress, fin_anal) =
      fin_anal ress
      @@ fun b_co_calls v_id v_arity ->
      let fst_inc_ar = ( + ) 1 @@ take_safety_into_acc unsafety v_id b_co_calls v_arity in
      anal_tt0 ~fst_inc_ar conts int f [ a ] ress |> ret_with_fv
    and fin_int_triv_bnd_anal ?(had_upd = false) conts int t (ress, fin_anal) =
      fin_anal ress
      @@ fun b_co_calls v_id v_arity ->
      let ress2 = if had_upd then ress else add_if_incr v_arity t ress in
      anal_triv conts int v_arity t ress2 |> ret_with_fv_cond v_id v_arity b_co_calls
    and fin_unint_bnd_anal anal_rhs (b_co_calls, b_ars, ress) =
      let b_fv = domain b_ars in
      let rhs_ars, ress2 = anal_rhs ress in
      let rhs_fv = domain rhs_ars in
      let p_ars = ar_union b_ars rhs_ars in
      let p_co_calls =
        union b_co_calls @@ union (cartesian_square rhs_fv) @@ cartesian rhs_fv b_fv
      in
      p_co_calls, p_ars, ress2
    and lam_call_anal lam_b conts int inc_ar ress pat t =
      match pat, t with
      | CPVar { id; _ }, t when is_int_triv_rhs int t ->
        fin_int_triv_bnd_anal conts int t
        @@ anal_bnd_cont id
        @@ anal_p conts ress inc_ar lam_b
        @@ ISet.add id int
      | _, t ->
        fin_unint_bnd_anal (fun ress' -> anal_triv conts int inc_ar t ress' |> ignore_fst)
        @@ anal_p conts ress inc_ar lam_b int
    and fin_lam_call_bnd_anal ~had_upd c lam_b conts int pat t unsafety rf =
      let ress, fin_anal = rf in
      fin_anal ress
      @@ fun b_co_calls v_id v_arity ->
      let inc_ar = take_safety_into_acc unsafety v_id b_co_calls v_arity in
      let ress2 = if had_upd then ress else add_if_pos c.id inc_ar ress in
      lam_call_anal lam_b conts int inc_ar ress2 pat t
      |> ret_with_fv_cond v_id v_arity b_co_calls
    and anal_cif c th el conts int ress inc_ar =
      let c_co_calls, c_ars, ress2 = anal_triv conts int 0 c ress in
      let th_co_calls, th_ars, ress3 = anal_p conts ress2 inc_ar th int in
      let el_co_calls, el_ars, ress4 = anal_p conts ress3 inc_ar el int in
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
    and anal_triv conts int inc_ar t ress =
      match t, inc_ar with
      | UVar { id; _ }, _ when ISet.mem id int -> empty, IMap.singleton id inc_ar, ress
      | (TUnit | TConst _ | UVar _), _ -> empty, IMap.empty, ress
      | TTuple (t1, t2, tt), _ -> anal_tt0 conts int t1 (t2 :: tt) ress
      | TSafeBinop (_, t1, t2), _ -> anal_tt0 conts int t1 [ t2 ] ress
      | Lam (_, _, lam_b), 0 ->
        let _, b_ars, ress2 = anal_p conts ress 0 lam_b int in
        cartesian_square @@ domain b_ars, b_ars, ress2
      | Lam (_, _, lam_b), _ ->
        let inc_ar2 = Int.max 0 (inc_ar - 1) in
        anal_p conts ress inc_ar2 lam_b int
    and anal_tt0 ?(fst_inc_ar = 0) conts int t1 tt ress =
      let anal_triv_sh = anal_triv conts int 0 in
      let f ((cc, ars, ress), (fv1, fv_acc)) t =
        let cc_t, ars_t, ress2 = anal_triv_sh t ress in
        let fv_acc2 = fv1 @@@ fv_acc in
        let fv_t = domain ars_t in
        let cc2 = union cc @@ union cc_t @@ cartesian fv_acc2 fv_t in
        (cc2, ar_union ars ars_t, ress2), (fv_t, fv_acc)
      in
      let init =
        let ((_, ars_t, _) as frst) = anal_triv conts int fst_inc_ar t1 ress in
        frst, (IMap.empty, domain ars_t)
      in
      List.fold_left f init tt |> fst
    in
    let open IMap in
    anal_p empty { dead_vars = ISet.empty; call_ars = empty } 0 p ISet.empty
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
        let counts' =
          match find id counts with
          | n -> add id (n + 1) counts
          | exception Not_found -> counts
        in
        k counts'
      | TTuple (t1, t2, tt) -> foldd_k anal_t (t1 :: t2 :: tt) k counts
      | TUnit | TConst _ -> k counts
    in
    let k _ _ counts barriers fin_call_ars = dead_vars, counts, barriers, fin_call_ars in
    anal_p p 0 k empty ISet.empty empty
  ;;

  module IIOPair = struct
    let pair a b = a, b

    let ( @@? ) f = function
      | None, b2 -> b2
      | (Some _ as b1), None -> b1
      | Some b1, Some b2 -> Option.some @@ f b1 b2
    ;;

    let map2 f (a1, b1) (a2, b2) = f a1 a2, f @@? (b1, b2)

    let ( +.+ ) (a, b) c =
      let b' =
        match b with
        | None -> None
        | Some b -> Some (b + c)
      in
      a + c, b'
    ;;

    let min = map2 min
  end

  (* s_ars elemet is a pair: max safe arity in terms of comp sharing and min unsafe arity in terms of side eff (None means no side effects) *)
  type safe_ars_elem = int * int option

  type min_prev_res =
    { rhs_res : safe_ars_elem
    ; b_res : safe_ars_elem
    }

  type conts_elem =
    | CPTupleContJV of safe_ars_elem
    | CPVarContJV of int * p * min_prev_res option

  let calc_safe_ars (_, _, p) =
    let open Option in
    let open Monads.Store in
    let open IMap in
    let open Base.Fn in
    let drop_conts ((fst, _), snd) = fst, snd in
    let u_return x () = return x in
    let upd_bnd_unsafities f =
      let* bnd_unsafeties, conts = get in
      let bnd_unsafeties', res = f bnd_unsafeties in
      put (bnd_unsafeties', conts) >>= u_return res
    in
    let add_cont id cont =
      let* bnd_unsafeties, conts = get in
      put (bnd_unsafeties, add id cont conts)
    in
    let inc n = n + 1 in
    let upd pat m el =
      match pat with
      | CPTuple _ -> m
      | CPVar { id; _ } -> add id el m
    in
    let sub ?(substr = 1) x_opt =
      match x_opt with
      | None -> None
      | Some x -> Option.some @@ Int.max 0 (x - substr)
    in
    let bnd_return n (_, se_s_ar) = return (n, se_s_ar) in
    let open IIOPair in
    let open IMap in
    let emp_run2 m bnd_unsafeties = run m (bnd_unsafeties, empty) in
    let rec check_p s_ars p n =
      match p with
      | CIf (_, th, el) ->
        let* th_res = check_p s_ars th n in
        let* el_res = check_p s_ars el n in
        min th_res el_res |> return
      | Let (Recursive, (CPVar { id; _ } as pat), rhs, b) ->
        run_check_t (add id (0, None) s_ars) n rhs
        >>= (function
         | (_, None) as rhs_res -> check_p (add id rhs_res s_ars) b n
         | rhs_res ->
           let* s_ars' = t_bnd (add id rhs_res s_ars) pat rhs in
           check_p s_ars' b n)
        >>= bnd_return n
      | Let (_, pat, rhs, b) | Ret (Cont (pat, b), rhs) ->
        let* s_ars' = t_bnd s_ars pat rhs in
        check_p s_ars' b n >>= bnd_return n
      | Primop (_, _, _, _, b) ->
        let* _ = check_p s_ars b n in
        return (n, Some n)
      | Letc (_, HALT, b) -> check_p s_ars b n
      | Letc ({ id = id'; _ }, CVar { id; _ }, b) ->
        let* bnd_unsafeties, conts = get in
        let conts' =
          match find id conts with
          | cont -> add id' cont conts
          | exception Not_found -> conts
        in
        put (bnd_unsafeties, conts') >>= fun () -> check_p s_ars b n
      | Letc ({ id = jv_id; _ }, Cont (CPVar { id; _ }, cont_b), b) ->
        add_cont jv_id (CPVarContJV (id, cont_b, none)) >>= fun () -> check_p s_ars b n
      | Letc ({ id; _ }, Cont (CPTuple _, cont_b), b) ->
        let* cont_b_res = check_p s_ars cont_b 0 in
        add_cont id (CPTupleContJV cont_b_res) >>= fun () -> check_p s_ars b n
      | Ret (HALT, t) -> run_check_t s_ars n t
      | Ret (CVar { id = cvar_id; _ }, t) ->
        let* rhs_res = run_check_t s_ars n t in
        let* _, conts = get in
        (match find cvar_id conts with
         | CPTupleContJV res -> res +.+ n |> bnd_return n
         | exception Not_found -> return rhs_res
         | CPVarContJV (id, b, prev_min) ->
           let fully_new_min =
             let* b_res = check_p (add id rhs_res s_ars) b n in
             add_cont cvar_id (CPVarContJV (id, b, Some { rhs_res; b_res }))
             >>= fun () -> bnd_return n b_res
           in
           (match prev_min with
            | None -> fully_new_min
            | Some prev_min ->
              let new_min = min rhs_res prev_min.rhs_res in
              if new_min = rhs_res
              then fully_new_min
              else if new_min = prev_min.rhs_res
              then bnd_return n prev_min.b_res
              else
                let* min_b_res = check_p (add id new_min s_ars) b n in
                let* () =
                  add_cont
                    cvar_id
                    (CPVarContJV (id, b, Some { rhs_res = new_min; b_res = min_b_res }))
                in
                check_p (add id rhs_res s_ars) b n >>= bnd_return n))
      | Call (f, a, c) ->
        let f_a_hndl =
          let* s_ars' =
            match f with
            | Lam (pat, _, _) -> t_bnd s_ars pat a
            | _ -> run_check_t s_ars n a >>= fun _ -> return s_ars
          in
          let* f_s_ar, f_se_s_ar = run_check_t s_ars' 0 f in
          return (f_s_ar - 1, sub f_se_s_ar)
        in
        let rid_off_neg_s_ar (s_ar, se_s_ar) = return (Int.max 0 s_ar, se_s_ar) in
        let last_call =
          let* rhs_res = f_a_hndl >>= rid_off_neg_s_ar in
          rhs_res +.+ n |> bnd_return n
        in
        let has_side_eff res = snd res = Some 0 in
        let fin_tuple_bnd b_res =
          f_a_hndl
          >>= compose
                (function
                  | true -> return (n, some n)
                  | false -> return (n, b_res +.+ n |> snd))
                has_side_eff
        in
        let b_hndl id b rhs_res =
          let* rhs_res' = rid_off_neg_s_ar rhs_res in
          let* b_res = check_p (add id rhs_res' s_ars) b n in
          return (rhs_res, b_res)
        in
        let fin_var_bnd ?(not_upd = false) id (rhs_res, b_res) =
          let upd =
            if has_side_eff rhs_res && not not_upd
            then
              upd_bnd_unsafities
              @@ fun bnd_unsafeties ->
              ( add
                  id
                  (match b_res with
                   | _, Some se_s_ar when se_s_ar <= n -> SideEffUnsafe
                   | _ -> Unsafe)
                  bnd_unsafeties
              , () )
            else if fst rhs_res <= 0 && not not_upd
            then
              upd_bnd_unsafities (fun bnd_unsafeties -> add id Unsafe bnd_unsafeties, ())
            else return ()
          in
          let calc_res () =
            return @@ if has_side_eff rhs_res then n, some n else n, snd b_res
          in
          upd >>= calc_res
        in
        (match c with
         | HALT -> last_call
         | Cont (CPVar { id; _ }, b) -> f_a_hndl >>= b_hndl id b >>= fin_var_bnd id
         | Cont (CPTuple _, b) -> check_p s_ars b 0 >>= fin_tuple_bnd
         | CVar { id = cvar_id; _ } ->
           let* _, conts = get in
           (match find cvar_id conts with
            | CPTupleContJV res -> fin_tuple_bnd res
            | exception Not_found -> last_call
            | CPVarContJV (id, b, prev_min) ->
              let* rhs_res = f_a_hndl in
              let fully_new_min =
                let* ((rhs_res, b_res) as ress) = b_hndl id b rhs_res in
                add_cont cvar_id (CPVarContJV (id, b, Some { rhs_res; b_res }))
                >>= fun () -> fin_var_bnd id ress
              in
              (match prev_min with
               | None -> fully_new_min
               | Some prev_min ->
                 let new_min = min rhs_res prev_min.rhs_res in
                 if new_min = rhs_res
                 then fully_new_min
                 else if new_min = prev_min.rhs_res
                 then fin_var_bnd ~not_upd:true id (rhs_res, prev_min.b_res)
                 else
                   let* new_min' = rid_off_neg_s_ar new_min in
                   let* min_b_res = check_p (add id new_min' s_ars) b n in
                   let* () =
                     add_cont
                       cvar_id
                       (CPVarContJV (id, b, Some { rhs_res = new_min; b_res = min_b_res }))
                   in
                   b_hndl id b rhs_res >>= fin_var_bnd ~not_upd:true id)))
    and check_t s_ars n = function
      | UVar { id; _ } -> return (find_with_default id s_ars (0, Some 0) +.+ n)
      | Lam (_, _, b) ->
        let* bnd_unsafeties = get in
        let bnd_unsafeties', b_res =
          bnd_unsafeties |> compose drop_conts @@ emp_run2 @@ check_p s_ars b @@ inc n
        in
        let* () = put bnd_unsafeties' in
        return b_res
      | _ -> return (n, none)
    and run_check_t s_ars n t = upd_bnd_unsafities @@ run @@ check_t s_ars n t
    and t_bnd s_ars pat rhs =
      upd_bnd_unsafities
      @@ run
      @@ let* rhs_res = check_t s_ars 0 rhs in
         upd pat s_ars rhs_res |> return
    in
    fst @@ fst @@ emp_run2 (check_p empty p 0) empty
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

  let call_arity_anal cps_prog =
    calc_safe_ars cps_prog |> anal cps_prog |> down_anal cps_prog |> simpl cps_prog
  ;;

  let call_arity_anal_debug cps_prog =
    let bnd_unsafeties = calc_safe_ars cps_prog in
    Printf.printf "bnd_unsafeties:\n";
    IMap.iter
      (fun id uns -> Printf.printf "%d: %s\n" id @@ show_bnd_unsafety uns)
      bnd_unsafeties;
    let ((_, upw_call_ars) as upw_res) = anal cps_prog bnd_unsafeties in
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
  (* TODO: printer above creates non-closed boxes.
    We need to fix it. Flush is a temporary workaround. *)
  Format.printf "%!";
  call_arity_anal cps_prog |> Format.printf "after:\n%a\n" MACPS.pp_vb;
  Format.printf "%!";
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
    let main =
              let f x k1 = k1 (fun y k2 -> k2 (x + y)) in
              if true then f 1 (fun g -> g 1 (fun x -> x))
              else f 2 (fun t -> t 1 (fun x -> x))
    after:
    let main =
             let f x e1 k1 = k1 (x + e1) in
             if true then f 1 1 (fun x -> x) else f 2 1 (fun x -> x)
    |}]
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
    let main =
              let f x k1 = k1 (fun y k2 -> k2 (x + y)) in
              let h a k3 = a 1 k3 in
              if true then f 1 (fun g -> g 1 (fun x -> x))
              else f 1 (fun t -> h t (fun x -> x))
    after:
    let main =
             let f x k1 = k1 (fun y k2 -> k2 (x + y)) in
             if true then f 1 (fun g -> g 1 (fun x -> x))
             else f 1 (fun t -> t 1 (fun x -> x))
    |}]
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
    let main =
              let f x k1 = k1 (fun y k2 -> k2 (fun a k3 -> k3 (x, x, y + a))) in
              f
                (1, 1, 1)
                (fun g -> if true then g 1 (fun t -> t 1 (fun x -> x))
                          else g 1 (fun b -> b 1 (fun x -> x)))

    after:
    let main =
             (fun g -> if true then g 1 1 (fun x -> x) else g 1 1 (fun x -> x))
             (fun e1 e2 k3 ->
               (fun x k1 -> k1 (x, x, e1 + e2)) (1, 1, 1) k3)
    |}]
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
    let main =
              let f x k1 =
                let h b k3 = k3 (b + b) in
                h x (fun l -> k1 (fun y k2 -> h y (fun r -> k2 (l + r))))
                in
              if true
              then f
                     1
                     (fun g -> g 1 (fun t -> g 1 (fun q -> (fun x -> x) (q + t))))

              else f
                     1
                     (fun g -> g 1 (fun s -> g 1 (fun z -> (fun x -> x) (z + s))))

    after:
    let main =
             let f x k1 =
               let h b k3 = k3 (b + b) in
               h x (fun l -> k1 (fun y k2 -> h y (fun r -> k2 (l + r))))
               in
             if true
             then f
                    1
                    (fun g -> g 1 (fun t -> g 1 (fun q -> (fun x -> x) (q + t))))

             else f
                    1
                    (fun g -> g 1 (fun s -> g 1 (fun z -> (fun x -> x) (z + s))))
    |}]
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
    let main =
              let f x k1 =
                let h b k3 = k3 (b + b) in
                h x (fun l -> k1 (fun y k2 -> h y (fun r -> k2 (l + r))))
                in
              if true
              then f
                     1
                     (fun g -> g 1 (fun t -> g 1 (fun q -> (fun x -> x) (t + t))))

              else f
                     1
                     (fun g -> g 1 (fun s -> g 1 (fun z -> (fun x -> x) (s + s))))

    after:
    let main =
             let f x e1 k1 =
               let h b k3 = k3 (b + b) in
               h x (fun l -> h e1 (fun r -> k1 (l + r)))
               in
             if true then f 1 1 (fun t -> (fun x -> x) (t + t))
             else f 1 1 (fun s -> (fun x -> x) (s + s))
    |}]
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
    let main =
              (fun x k1 ->
                let h b k3 = k3 (x, b) in
                h x (fun l -> k1 (fun y k2 -> h y (fun r -> k2 (l, r)))))
                (1 + 1)
                (fun g -> g 1 (fun t -> g 1 (fun q -> (fun x -> x) (q + t))))

    after:
    let main =
             (fun x k1 ->
               let h b k3 = k3 (x, b) in
               h x (fun l -> k1 (fun y k2 -> h y (fun r -> k2 (l, r)))))
               (1 + 1)
               (fun g -> g 1 (fun t -> g 1 (fun q -> (fun x -> x) (q + t))))
    |}]
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
    let main =
              (fun x k1 ->
                let h b k3 = k3 (x, b) in
                h x (fun l -> k1 (fun y k2 -> h y (fun r -> k2 (l, r)))))
                (1 + 1)
                (fun g -> g 1 (fun t -> g 1 (fun q -> (fun x -> x) (q + q))))

    after:
    let main =
             (fun x k1 ->
               let h b k3 = k3 (x, b) in
               h x (fun l -> h 1 (fun r -> k1 (l, r))))
               (1 + 1)
               (fun q -> (fun x -> x) (q + q))
    |}]
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
    let main =
              let f x k1 =
                if true then k1 (fun y k2 -> k2 (x + y))
                else k1 (fun a k3 -> k3 (x + a))
                in
              if true
              then f
                     1
                     (fun g -> g 1 (fun t -> g 1 (fun q -> (fun x -> x) (t + q))))

              else f
                     1
                     (fun g -> g 1 (fun s -> g 1 (fun z -> (fun x -> x) (s + z))))

    after:
    let main =
             let f x e1 k1 = if true then k1 (x + e1) else k1 (x + e1) in
             if true
             then (fun g -> g 1 (fun t -> g 1 (fun q -> (fun x -> x) (t + q))))
                  (fun e4 k5 ->
                    f 1 e4 k5)

             else (fun g -> g 1 (fun s -> g 1 (fun z -> (fun x -> x) (s + z))))
                  (fun e2 k3 ->
                    f 1 e2 k3)
    |}]
;;

let%expect_test
    " expand call thanks to fake shared comput. and cheap exprs detection. LamCall \
     version"
  =
  let rhs =
    let sum var2 = sum (UVar var_x) (UVar var2) in
    let th = Ret (CVar var_k1, Lam (CPVar var_y, var_k2, Ret (CVar var_k2, sum var_y))) in
    let el = Ret (CVar var_k1, Lam (CPVar var_a, var_k3, Ret (CVar var_k3, sum var_a))) in
    Lam (CPVar var_x, var_k1, CIf (tr, th, el))
  in
  let th =
    let cont2 = Cont (CPVar var_q, Ret (CVar var_k4, sum (UVar var_t) (UVar var_q))) in
    let cont1 = Cont (CPVar var_t, Call (UVar var_g, one, cont2)) in
    Call (UVar var_f, one, Cont (CPVar var_g, Call (UVar var_g, one, cont1)))
  in
  let el =
    let cont2 = Cont (CPVar var_z, Ret (CVar var_k4, sum (UVar var_s) (UVar var_z))) in
    let cont1 = Cont (CPVar var_s, Call (UVar var_g, one, cont2)) in
    Call (UVar var_f, one, Cont (CPVar var_g, Call (UVar var_g, one, cont1)))
  in
  test_call_ar_anal
  @@ prog
  @@ Call (Lam (CPVar var_f, var_k4, CIf (tr, th, el)), rhs, HALT);
  [%expect
    {|
    before:
    let main =
              (fun f k4 ->
                if true
                then f 1 (fun g -> g 1 (fun t -> g 1 (fun q -> k4 (t + q))))
                else f 1 (fun g -> g 1 (fun s -> g 1 (fun z -> k4 (s + z)))))
                (fun x k1 ->
                  if true then k1 (fun y k2 -> k2 (x + y))
                  else k1 (fun a k3 -> k3 (x + a)))
                (fun x -> x)

    after:
    let main =
             (fun f k4 ->
               if true
               then (fun g -> g 1 (fun t -> g 1 (fun q -> k4 (t + q)))) (fun e3 k4 ->
                                                                        f 1 e3 k4)

               else (fun g -> g 1 (fun s -> g 1 (fun z -> k4 (s + z)))) (fun e1 k2 ->
                                                                        f 1 e1 k2))
               (fun x e5 k1 -> if true then k1 (x + e5) else k1 (x + e5))
               (fun x -> x)
    |}]
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
    let main =
              let f x k1 = k1 (fun y k2 -> k2 (x, y)) in
              if true
              then (fun h k3 -> h 1 (fun s -> s 1 k3))
                     (fun t k4 -> f (1, t) k4)
                     (fun x -> x)
               else f (1, 1) (fun g -> g 1 (fun x -> x))
    after:
    let main =
             let f x e1 k1 = k1 (x, e1) in
             if true then f (1, 1) 1 (fun x -> x) else f (1, 1) 1 (fun x -> x)
    |}]
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
    let main =
              let f x k1 = k1 (fun y k2 -> k2 (x, y)) in
              if true
              then (fun h k3 ->
                     if true then h 1 (fun s -> s 1 k3)
                     else h 1 (fun q -> q 1 k3))
                     (fun t k4 -> f (1, t) k4)
                     (fun x -> x)
               else f (1, 1) (fun g -> g 1 (fun x -> x))
    after:
    let main =
             let f x e1 k1 = k1 (x, e1) in
             if true
             then (fun h k3 -> if true then h 1 1 k3 else h 1 1 k3)
                    (fun t e2 k4 -> f (1, t) e2 k4)
                    (fun x -> x)
              else f (1, 1) 1 (fun x -> x)
    |}]
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
    let main =
              let jv1 f = f 1 (fun g -> g 1 (fun x -> x)) in if true
                                                             then jv1 (fun x k1 ->
                                                                        k1
                                                                        (fun y k2 ->
                                                                        k2 (x + y)))

                                                             else jv1 (fun l k3 ->
                                                                        k1
                                                                        (fun r k4 ->
                                                                        k2 (l + r)))
    after:
    let main =
             let jv1 f = f 1 1 (fun x -> x) in if true
                                               then jv1 (fun x e2 k1 ->
                                                          k1 (x + e2))

                                               else jv1 (fun l e1 k3 ->
                                                          k1 (l + e1))
    |}]
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
    let main =
              let jv1 f = (fun x -> x) (1 + 1) in if true
                                                  then jv1 (fun x k1 ->
                                                             k1 (fun y k2 ->
                                                                  k2 (x + y)))

                                                  else jv1 (fun l k3 ->
                                                             k1 (fun r k4 ->
                                                                  k2 (l + r)))
    after:
    let main =
             let jv1 f = (fun x -> x) (1 + 1) in if true then jv1 () else jv1 ()
    |}]
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
    let main =
              let f x k1 =
                let h a k3 = k3 (fun b k4 -> k4 (a, b)) in
                k1 (fun y k2 -> if y then k2 h else k2 h)
                in
              f
                ()
                (fun r -> r
                            (2 <= 1)
                            (fun g -> g (1, 2) (fun t -> t 2 (fun x -> x)))
                         )

    after:
    let main =
             let h a e1 k3 = k3 (a, e1) in
             (fun e2 k2 -> if 2 <= 1 then h e2 2 k2 else h e2 2 k2)
               (1, 2)
               (fun x -> x)
    |}]
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
    let main =
              let rec fack n k1 =
                if n <= 1 then k1 (fun k k2 -> k 1 k2)
                else fack
                       (n - 1)
                       (fun h -> k1 (fun k k3 -> h k (fun t -> k (t * n) k3)))

                in
              fack 1 (fun g -> g (fun x k4 -> k4 x) (fun x -> x))
    after:
    let main =
             let rec fack n e1 k1 =
               if n <= 1 then e1 1 k1
               else fack (n - 1) e1 (fun t -> e1 (t * n) k1)
               in
             fack 1 (fun x k4 -> k4 x) (fun x -> x)
    |}]
;;

let%expect_test "fibk" =
  test_call_ar_anal
  @@ Result.get_ok
  @@ CPSConv.cps_conv_program
  @@ Result.get_ok
  @@ Frontend.Parsing.parse_structure
       "let main  = let rec fibk n = if n <=1 then fun k -> k 1 else let h = fibk (n-1) \
        in fun k -> h (fun l -> fibk (n-2) (fun r -> k (l + r)) ) in fibk 1 (fun x -> x)";
  [%expect
    {|
    before:
    let main =
              let rec fibk n k1 =
                if n <= 1 then k1 (fun k k2 -> k 1 k2)
                else fibk
                       (n - 1)
                       (fun t3 -> k1 (fun k k4 ->
                                       t3
                                         (fun l k5 ->
                                           fibk
                                             (n - 2)
                                             (fun t6 -> t6
                                                          (fun r k7 ->
                                                            k (l + r) k7)
                                                          k5
                                                       )
                                           )
                                         k4
                                       ))

                in
              fibk
                1
                (fun t8 -> t8 (fun x k9 -> k9 x) (fun t10 -> (fun x -> x) t10))

    after:
    let main =
             let rec fibk n e11 k1 =
               if n <= 1 then e11 1 k1
               else fibk
                      (n - 1)
                      (fun l k5 -> fibk (n - 2) (fun r k7 -> e11 (l + r) k7) k5)
                      k1

               in
             fibk 1 (fun x k9 -> k9 x) (fun t10 -> (fun x -> x) t10)
    |}]
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
    let main =
              let g x k1 =
                let h b k4 = k4 (b + b) in
                h
                  x
                  (fun l -> k1 (fun z k2 ->
                                 k2 (fun y k3 -> h y (fun r -> k3 (z + (l + r))))))

                in
              let rec q s k5 =
                let u a k8 = k8 (a + a) in
                u
                  s
                  (fun n -> k5 (fun j k6 ->
                                 k6 (fun c k7 -> u c (fun d -> k7 (j + (n + d))))))

                in
              let rec f t k9 = if true then q t k9 else g t k9 in
              f 1 (fun v -> v 1 (fun w -> w 1 (fun m -> (fun x -> x) (m, g))))
    after:
    let main =
             let g x k1 =
               let h b k4 = k4 (b + b) in
               h
                 x
                 (fun l -> k1 (fun z k2 ->
                                k2 (fun y k3 -> h y (fun r -> k3 (z + (l + r))))))

               in
             let rec q s e2 e1 k5 =
               let u a k8 = k8 (a + a) in
               u s (fun n -> u e1 (fun d -> k5 (e2 + (n + d))))
               in
             let rec f t k9 =
               if true then k9 (fun e5 k6 -> k6 (fun e3 k4 -> q t e5 e3 k4))
               else g t k9
               in
             f 1 (fun v -> v 1 (fun w -> w 1 (fun m -> (fun x -> x) (m, g))))
    |}]
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
    let main =
              (fun x k1 -> if true then k1 1 else k1 2)
                (fun a k3 -> k3 (fun b k4 -> k4 (a, b)))
                (fun y -> (fun x -> x) (y, y))

    after:
    let main =
             (fun x k1 -> if true then k1 1 else k1 2)
               ()
               (fun y -> (fun x -> x) (y, y))
    |}]
;;

let call_arity_anal = call_arity_anal
