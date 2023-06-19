(* http://dev.stephendiehl.com/fun/006_hindley_milner.html *)

open Base
open Typedtree
module Format = Caml.Format (* silencing a warning *)

(* let use_logging = false *)
let use_logging = true

let log fmt =
  if use_logging
  then Format.kasprintf (fun s -> Format.printf "%s\n%!" s) fmt
  else Format.ifprintf Format.std_formatter fmt
;;

type error =
  [ `Occurs_check
  | `NoVariable of string
  | `UnificationFailed of ty * ty
  ]

let pp_error ppf : error -> _ = function
  | `Occurs_check -> Format.fprintf ppf "Occurs check failed"
  | `NoVariable s -> Format.fprintf ppf "Undefined variable '%s'" s
  | `UnificationFailed (l, r) ->
    Format.fprintf ppf "unification failed on %a and %a" Pprint.pp_typ l Pprint.pp_typ r
;;

type fresh_counter = int

module R : sig
  type 'a t

  val bind : 'a t -> f:('a -> 'b t) -> 'b t
  val return : 'a -> 'a t
  val fail : error -> 'a t

  include Monad.Infix with type 'a t := 'a t

  module Syntax : sig
    val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
    val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
  end

  (** Creation of a fresh name from internal state *)
  val fresh : int t

  val level : int t
  val enter_level : unit t
  val leave_level : unit t

  (** Running a transformer: getting the inner result value *)
  val run : 'a t -> ('a, error) Result.t

  val list_foldm : f:('a -> 'b -> 'a t) -> init:'a t -> 'b list -> 'a t
end = struct
  type cur_level = int

  (* A compositon: State monad after Result monad *)
  type 'a t =
    fresh_counter * cur_level -> (fresh_counter * cur_level) * ('a, error) Result.t

  let ( >>= ) : 'a 'b. 'a t -> ('a -> 'b t) -> 'b t =
   fun m f st ->
    let last, r = m st in
    match r with
    | Result.Error x -> last, Error x
    | Ok a -> f a last
 ;;

  let fail e st = st, Result.fail e
  let return x last = last, Result.return x
  let bind x ~f = x >>= f

  let ( >>| ) : 'a 'b. 'a t -> ('a -> 'b) -> 'b t =
   fun x f st ->
    match x st with
    | st, Ok x -> st, Ok (f x)
    | st, Result.Error e -> st, Result.Error e
 ;;

  module Syntax = struct
    let ( let* ) x f = bind x ~f
    let ( let+ ) = ( >>| )
  end

  let fresh : int t =
   fun (last_fresh, level) -> (last_fresh + 1, level), Result.Ok last_fresh
 ;;

  let level : int t = fun ((_, level) as info) -> info, Result.Ok level

  (* let set_level : int -> unit t = fun n (fresh, _) -> (fresh, n), Result.Ok ()
      *)
  let enter_level : unit t = fun (fresh, level) -> (fresh, 1 + level), Result.Ok ()
  let leave_level : unit t = fun (fresh, level) -> (fresh, level - 1), Result.Ok ()
  let run : 'a. 'a t -> ('a, error) Result.t = fun m -> snd (m (0, 0))

  let rec list_foldm ~f ~init xs =
    let open Syntax in
    match xs with
    | [] -> init
    | h :: tl ->
      let* acc = init in
      list_foldm ~f ~init:(f acc h) tl
  ;;
end

module Subst : sig
  type t

  val pp : Caml.Format.formatter -> t -> unit
  val empty : t
  val singleton : binder -> ty -> t

  (** Getting value from substitution *)
  val find_exn : binder -> t -> ty

  val find : fresh_counter -> t -> ty option
  val apply : t -> ty -> ty

  (** Compositon of substitutions *)
  val ( ++ ) : t -> t -> t

  (** Alias for [(++)] *)
  val compose : t -> t -> t

  val remove : t -> binder -> t
end = struct
  (* an association list. In real world replace it by Map *)
  type t = (fresh_counter * ty) list

  let pp ppf subst =
    let open Format in
    fprintf
      ppf
      "[ %a ]"
      (pp_print_list
         ~pp_sep:(fun ppf () -> fprintf ppf ", ")
         (fun ppf (k, v) -> fprintf ppf "%d -> %a" k Pprint.pp_typ v))
      subst
  ;;

  let empty = []
  let singleton k v = [ k, v ]
  let find_exn k xs = List.Assoc.find_exn xs k ~equal:Int.equal
  let find k xs = List.Assoc.find xs k ~equal:Int.equal
  let remove xs k = List.Assoc.remove xs k ~equal:Int.equal

  let apply s =
    let rec helper typ =
      match typ.typ_desc with
      | V { binder; _ } ->
        (match find_exn binder s with
         | exception Not_found_s _ -> typ
         | x -> x)
      | Arrow (l, r) -> tarrow (helper l) (helper r)
      | _ -> typ
    in
    helper
  ;;

  let union : t -> t -> t =
   fun xs ys ->
    List.fold_left ys ~init:xs ~f:(fun acc (k, v) ->
      match List.Assoc.find acc ~equal:Int.equal k with
      | None -> (k, v) :: acc
      | Some _x -> acc)
 ;;

  let compose s1 s2 = union (List.Assoc.map s2 ~f:(apply s1)) s1
  let ( ++ ) = compose
end

module Var_set = struct
  include Var_set

  let fold_R f acc set =
    fold
      (fun x acc ->
        let open R.Syntax in
        let* acc = acc in
        f acc x)
      acc
      set
  ;;
end

module Type = struct
  type t = ty

  let occurs_in info =
    let exception Occurs in
    let rec helper wher : unit =
      match wher.typ_desc with
      | V { binder; _ } when binder = info.Typedtree.binder -> raise Occurs
      | V ({ var_level; _ } as v) ->
        let min_level = Int.min var_level info.var_level in
        v.var_level <- min_level
      | Arrow (l, r) ->
        helper l;
        helper r
      | TProd (a, b, ts) ->
        helper a;
        helper b;
        List.iter ts ~f:helper
      | TLink t -> helper t
      | Prim _ -> ()
    in
    fun wher ->
      try
        helper wher;
        false
      with
      | Occurs -> true
  ;;

  let free_vars =
    let rec helper acc { typ_desc } =
      match typ_desc with
      | Prim _ -> acc
      | V { binder; _ } -> Var_set.add binder acc
      | TLink t -> helper acc t
      | Arrow (l, r) -> helper (helper acc l) r
      | TProd (a, b, ts) -> List.fold_left ts ~init:(helper (helper acc a) b) ~f:helper
    in
    helper Var_set.empty
  ;;

  let apply subs t = Subst.apply subs t
end

module Scheme = struct
  type t = scheme

  let occurs_in info = function
    | S (xs, t) -> (not (Var_set.mem info.binder xs)) && Type.occurs_in info t
  ;;

  let free_vars = function
    | S (bs, t) -> Var_set.fold Var_set.remove bs (Type.free_vars t)
  ;;

  let apply sub (S (names, ty)) =
    let s2 = Var_set.fold (fun k s -> Subst.remove s k) names sub in
    S (names, Type.apply s2 ty)
  ;;

  let pp = Pprint.pp_scheme
end

let%expect_test " " =
  Format.printf "%a\n" Var_set.pp (Type.free_vars (tv 1 ~level:1));
  Format.printf "%a\n" Var_set.pp (Scheme.free_vars (S (Var_set.empty, tv 1 ~level:1)));
  [%expect {|
    [ 1; ]
    [ 1; ] |}]
;;

module TypeEnv = struct
  type t = (string * scheme) list

  let extend e h = h :: e
  let empty = []

  let free_vars : t -> Var_set.t =
    List.fold_left ~init:Var_set.empty ~f:(fun acc (_, s) ->
      Var_set.union acc (Scheme.free_vars s))
  ;;

  let apply s env = List.Assoc.map env ~f:(Scheme.apply s)

  let pp ppf xs =
    Caml.Format.fprintf ppf "{| ";
    List.iter xs ~f:(fun (n, s) ->
      Caml.Format.fprintf ppf "%s -> %a; " n Pprint.pp_scheme s);
    Caml.Format.fprintf ppf "|}%!"
  ;;

  let find_exn name xs = List.Assoc.find_exn ~equal:String.equal xs name
end

open R
open R.Syntax

let unify l r =
  let rec helper l r =
    match l.typ_desc, r.typ_desc with
    | TLink l, _ -> helper l r
    | _, TLink r -> helper l r
    | Prim l, Prim r when String.equal l r -> return ()
    | Prim _, Prim _ -> fail (`UnificationFailed (l, r))
    | V { binder = a; _ }, V { binder = b; _ } when Int.equal a b -> return ()
    | V info, _ when Type.occurs_in info r -> fail `Occurs_check
    | V _, _ ->
      l.typ_desc <- TLink r;
      return ()
    | _, V _ ->
      r.typ_desc <- TLink l;
      return ()
    | Arrow (l1, r1), Arrow (l2, r2) ->
      let* () = helper l1 l2 in
      helper r1 r2
    | TProd (a1, b1, ts1), TProd (a2, b2, ts2) ->
      let* () = helper a1 a2 in
      let* () = helper b1 b2 in
      if List.length ts1 = List.length ts2
      then
        List.fold2_exn ts1 ts2 ~init:(return ()) ~f:(fun acc l r ->
          let* () = acc in
          helper l r)
      else fail (`UnificationFailed (l, r))
    | TProd _, Arrow _
    | Arrow _, TProd _
    | TProd _, Prim _
    | Prim _, TProd _
    | Arrow _, Prim _
    | Prim _, Arrow _ -> fail (`UnificationFailed (l, r))
  in
  helper l r
;;

let instantiate : scheme -> ty R.t =
 fun (S (bs, t)) ->
  Var_set.fold_R
    (fun typ name ->
      let* f1 = fresh in
      return @@ Subst.apply (Subst.singleton name (tv f1 ~level:0)) typ)
    bs
    (return t)
;;

let generalize : TypeEnv.t -> level:int -> Type.t -> Scheme.t =
 fun env ~level ->
  let rec helper acc typ : binder_set =
    match typ.typ_desc with
    | V { var_level; binder } -> if var_level > level then Var_set.add binder acc else acc
    | TLink t -> helper acc t
    | Arrow (l, r) -> helper (helper acc l) r
    | TProd (a, b, tl) -> List.fold_left ~f:helper tl ~init:(helper (helper acc a) b)
    | Prim _ -> acc
  in
  fun ty ->
    log "generalize: @[%a@]" pp_ty ty;
    let free = helper Var_set.empty ty in
    (* let free = Var_set.diff (Type.free_vars ty) (TypeEnv.free_vars env) in *)
    S (free, ty)
;;

let lookup_env e xs =
  (* log "Looking up for %s" e;
  log "  inside %a" TypeEnv.pp xs; *)
  match List.Assoc.find_exn xs ~equal:String.equal e with
  | (exception Caml.Not_found) | (exception Not_found_s _) -> fail (`NoVariable e)
  | scheme -> instantiate scheme
;;

let fresh_var ~level = fresh >>| fun n -> tv n ~level

let pp_env subst ppf env =
  let env : TypeEnv.t =
    List.map ~f:(fun (k, S (args, v)) -> k, S (args, Subst.apply subst v)) env
  in
  TypeEnv.pp ppf env
;;

let infer env expr =
  let current_level = ref 1 in
  let enter_level () =
    log "== enter level %d" (1 + !current_level);
    Int.incr current_level
  in
  let leave_level () =
    log "== leave level %d" !current_level;
    Int.decr current_level
  in
  let rec (helper : TypeEnv.t -> Parsetree.expr -> (ty * Typedtree.expr) R.t) =
   fun env -> function
    | Parsetree.EVar ("*" as _v) | Parsetree.EVar ("-" as _v) | Parsetree.EVar ("+" as _v)
      ->
      let typ = tarrow int_typ (tarrow int_typ int_typ) in
      return (typ, TVar (_v, typ))
    | Parsetree.EVar "=" ->
      let typ = tarrow int_typ (tarrow int_typ bool_typ) in
      return (typ, TVar ("=", typ))
    | Parsetree.EVar x ->
      let* typ = lookup_env x env in
      return (typ, TVar (x, typ))
      (* lambda abstraction *)
    | EUnit -> return (unit_typ, TUnit)
    | ELam (PVar x, e1) ->
      let* v = fresh_var ~level:!current_level in
      let env2 = TypeEnv.extend env (x, S (Var_set.empty, v)) in
      let* ty, tbody = helper env2 e1 in
      let trez = tarrow v ty in
      return (trez, TLam (x, tbody, trez))
    | EApp (e1, e2) ->
      let* t1, te1 = helper env e1 in
      let* t2, te2 = helper env e2 in
      let* tv = fresh_var ~level:0 in
      let* () = unify t1 (tarrow t2 tv) in
      (* log "t1 = %a" pp_ty t1; *)
      (* log "t2 = %a" pp_ty t2; *)
      (* log "tv = %a" pp_ty tv; *)
      return (tv, TApp (te1, te2, tv))
    | EConst (PConst_int _n as c) -> return (int_typ, TConst c)
    | EConst (PConst_bool _b as c) -> return (bool_typ, TConst c)
    | Parsetree.EIf (c, th, el) ->
      let* t1, tc = helper env c in
      let* t2, tth = helper env th in
      let* t3, tel = helper env el in
      let* () = unify t1 bool_typ in
      let* () = unify t2 t3 in
      R.return (t2, TIf (tc, tth, tel, t2))
    | ETuple (a, b, es) ->
      (* fold_left *)
      let* ta, ea = helper env a in
      let* tb, eb = helper env b in
      let* typs, exprs =
        list_foldm
          ~init:(return ([], []))
          ~f:(fun (typs, exprs) e ->
            let* t1, e1 = helper env e in
            return (t1 :: typs, e1 :: exprs))
          es
      in
      let tup_typ = tprod ta tb typs in
      return (tup_typ, TTuple (ea, eb, exprs, tup_typ))
    | Parsetree.ELet (NonRecursive, PVar x, rhs, e2) ->
      enter_level ();
      let* t1, typed_rhs = helper env rhs in
      leave_level ();
      (* log "letrec t1 = %a" pp_ty t1; *)
      (* log "env = %a" TypeEnv.pp env; *)
      (* log "env free vars = %a" Var_set.pp (TypeEnv.free_vars env); *)
      let t2 = generalize env ~level:!current_level t1 in
      log "let generalized t2 = %a" Pprint.pp_scheme t2;
      let* t3, typed_in = helper (TypeEnv.extend env (x, t2)) e2 in
      log "let nonrec result = %a" pp_ty t3;
      return (t3, TLet (NonRecursive, x, t2, typed_rhs, typed_in))
    | Parsetree.ELet (Recursive, PVar f, erhs, wher) ->
      let* tv = fresh_var ~level:!current_level in
      enter_level ();
      let* t1, typed_rhs =
        let env = TypeEnv.extend env (f, S (Var_set.empty, tv)) in
        helper env erhs
      in
      leave_level ();
      let* () = unify tv t1 in
      let t2 = generalize env ~level:!current_level tv in
      log "letrec  result = %a\n%!" pp_scheme t2;
      let* twher, typed_wher = helper TypeEnv.(extend env (f, t2)) wher in
      return (twher, TLet (Recursive, f, t2, typed_rhs, typed_wher))
    | ELet (_, PTuple _, _, _) | Parsetree.ELam (PTuple _, _) -> assert false
  in
  helper env expr
;;

let start_env =
  TypeEnv.(extend empty) ("print", S (Var_set.empty, tarrow int_typ unit_typ))
;;

let w e =
  Result.map (run (infer start_env e)) ~f:(fun (_, x) -> x)
  |> Result.map_error ~f:(function #error as x -> x)
;;

let%expect_test _ =
  let _ =
    let tv1 = tv 1 ~level:0 in
    let tv2 = tv 2 ~level:0 in
    let l = tarrow tv1 tv1 in
    let r = tarrow (tprim "int") tv2 in
    let subst = unify l r in
    let open Caml.Format in
    match R.run subst with
    | Result.Error _ -> ()
    | Ok () ->
      Format.printf " [ 1 -> %a, 2 -> %a ] %!" Pprint.pp_typ tv1 Pprint.pp_typ tv2
  in
  [%expect {| [ 1 -> int, 2 -> int ] |}]
;;

let vb ?(env = start_env) (flg, Parsetree.PVar name, body) =
  (* TODO(Kakadu): recursion and generalization *)
  let comp =
    let* v = fresh in
    infer ((name, S (Var_set.empty, Typedtree.tv v ~level:(-1))) :: env) body
  in
  run comp
  |> Result.map_error ~f:(function #error as x -> x)
  |> Result.map ~f:(fun (ty, body) ->
       value_binding flg (PVar name) body (generalize env ~level:(-1) ty))
;;

let structure ?(env = start_env) stru =
  let ( let* ) = Result.( >>= ) in
  let return = Result.return in
  let* new_env, items =
    List.fold_left
      stru
      ~init:(return (env, []))
      ~f:(fun acc item ->
        let* env, acc = acc in
        let* new_item = vb ~env item in
        let env : TypeEnv.t =
          match new_item.Typedtree.tvb_pat with
          | Parsetree.PVar name -> TypeEnv.extend env (name, new_item.Typedtree.tvb_typ)
        in
        return (env, new_item :: acc))
  in
  return (List.rev items)
;;
