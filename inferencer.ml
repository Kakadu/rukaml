(* http://dev.stephendiehl.com/fun/006_hindley_milner.html *)

open Base
open Typedtree
module Format = Caml.Format (* silencing a warning *)

let use_logging = false
(* let use_logging = true *)

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

  (** Running a transformer: getting the inner result value *)
  val run : 'a t -> ('a, error) Result.t
end = struct
  (* A compositon: State monad after Result monad *)
  type 'a t = int -> int * ('a, error) Result.t

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

  let fresh : int t = fun last -> last + 1, Result.Ok last
  let run m = snd (m 0)
end

type fresh = int

module Subst : sig
  type t

  val pp : Caml.Format.formatter -> t -> unit
  val empty : t
  val singleton : fresh -> ty -> t

  (** Getting value from substitution *)
  val find_exn : fresh -> t -> ty

  val find : fresh -> t -> ty option
  val apply : t -> ty -> ty

  (** Compositon of substitutions *)
  val ( ++ ) : t -> t -> t

  (** Alias for [(++)] *)
  val compose : t -> t -> t

  val remove : t -> fresh -> t
end = struct
  (* an association list. In real world replace it by Map *)
  type t = (fresh * ty) list

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
      | V b ->
        (match find_exn b s with
         | exception Not_found_s _ -> { typ_desc = V b }
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

module VarSet = struct
  include VarSet

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

  let rec occurs_in v { typ_desc } =
    match typ_desc with
    | V b -> b = v
    | Arrow (l, r) -> occurs_in v l || occurs_in v r
    | TLink t -> occurs_in v t
    | Prim _ -> false
  ;;

  let free_vars =
    let rec helper acc { typ_desc } =
      match typ_desc with
      | Prim _ -> acc
      | V b -> VarSet.add b acc
      | TLink t -> helper acc t
      | Arrow (l, r) -> helper (helper acc l) r
    in
    helper VarSet.empty
  ;;

  let apply subs t = Subst.apply subs t
end

module Scheme = struct
  type t = scheme

  let occurs_in v = function
    | S (xs, t) -> (not (VarSet.mem v xs)) && Type.occurs_in v t
  ;;

  let free_vars = function
    | S (bs, t) -> VarSet.fold VarSet.remove bs (Type.free_vars t)
  ;;

  let apply sub (S (names, ty)) =
    let s2 = VarSet.fold (fun k s -> Subst.remove s k) names sub in
    S (names, Type.apply s2 ty)
  ;;

  let pp = Pprint.pp_scheme
end

let%expect_test " " =
  Format.printf "%a\n" VarSet.pp (Type.free_vars { typ_desc = V 1 });
  Format.printf "%a\n" VarSet.pp (Scheme.free_vars (S (VarSet.empty, { typ_desc = V 1 })));
  [%expect {|
    [ 1; ]
    [ 1; ] |}]
;;

module TypeEnv = struct
  type t = (string * scheme) list

  let extend e h = h :: e
  let empty = []

  let free_vars : t -> VarSet.t =
    List.fold_left ~init:VarSet.empty ~f:(fun acc (_, s) ->
      VarSet.union acc (Scheme.free_vars s))
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
    | V a, V b when Int.equal a b -> return ()
    | V b, _ when Type.occurs_in b r -> fail `Occurs_check
    | V _, _ ->
      l.typ_desc <- TLink r;
      return ()
    | _, V _ ->
      r.typ_desc <- TLink l;
      return ()
    | Arrow (l1, r1), Arrow (l2, r2) ->
      let* () = helper l1 l2 in
      helper r1 r2
    | Arrow _, Prim _ | Prim _, Arrow _ -> fail (`UnificationFailed (l, r))
  in
  helper l r
;;

let instantiate : scheme -> ty R.t =
 fun (S (bs, t)) ->
  VarSet.fold_R
    (fun typ name ->
      let* f1 = fresh in
      return @@ Subst.apply (Subst.singleton name (tv f1)) typ)
    bs
    (return t)
;;

let generalize : TypeEnv.t -> Type.t -> Scheme.t =
 fun env ty ->
  let free = VarSet.diff (Type.free_vars ty) (TypeEnv.free_vars env) in
  S (free, ty)
;;

let lookup_env e xs =
  match List.Assoc.find_exn xs ~equal:String.equal e with
  | (exception Caml.Not_found) | (exception Not_found_s _) -> fail (`NoVariable e)
  | scheme -> instantiate scheme
;;

let fresh_var = fresh >>| fun n -> { typ_desc = V n }

let pp_env subst ppf env =
  let env : TypeEnv.t =
    List.map ~f:(fun (k, S (args, v)) -> k, S (args, Subst.apply subst v)) env
  in
  TypeEnv.pp ppf env
;;

let infer =
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
    | ELam (PVar x, e1) ->
      let* v = fresh_var in
      let env2 = TypeEnv.extend env (x, S (VarSet.empty, v)) in
      let* ty, tbody = helper env2 e1 in
      let trez = tarrow v ty in
      return (trez, TLam (x, tbody, trez))
    | EApp (e1, e2) ->
      let* t1, te1 = helper env e1 in
      let* t2, te2 = helper env e2 in
      let* tv = fresh_var in
      let* () = unify t1 (tarrow t2 tv) in
      (* log "t1 = %a" pp_ty t1; *)
      (* log "t2 = %a" pp_ty t2; *)
      (* log "tv = %a" pp_ty tv; *)
      return (tv, TApp (te1, te2, tv))
    | EConst c -> return (int_typ, TConst c)
    | Parsetree.EIf (c, th, el) ->
      let* t1, tc = helper env c in
      let* t2, tth = helper env th in
      let* t3, tel = helper env el in
      let* () = unify t1 bool_typ in
      let* () = unify t2 t3 in
      R.return (t2, TIf (tc, tth, tel, t2))
    | Parsetree.ELet (NonRecursive, PVar x, rhs, e2) ->
      let* t1, trhs = helper env rhs in
      (* log "letrec t1 = %a" pp_ty t1; *)
      (* log "env = %a" TypeEnv.pp env; *)
      (* log "env free vars = %a" VarSet.pp (TypeEnv.free_vars env); *)
      let t2 = generalize env t1 in
      (* log "letrec t2 = %a" Pprint.pp_scheme t2; *)
      let* t3, typed_in = helper (TypeEnv.extend env (x, t2)) e2 in
      (* log "letrec result = %a" pp_ty t3; *)
      return (t3, TLet (NonRecursive, x, t2, trhs, typed_in))
    | Parsetree.ELet (Recursive, PVar f, erhs, wher) ->
      let* tv = fresh_var in
      let* t1, typed_rhs =
        let env = TypeEnv.extend env (f, S (VarSet.empty, tv)) in
        helper env erhs
      in
      let* () = unify tv t1 in
      let t2 = generalize env tv in
      let* twher, typed_wher = helper TypeEnv.(extend env (f, t2)) wher in
      return (twher, TLet (Recursive, f, t2, typed_rhs, typed_wher))
  in
  helper
;;

let start_env = TypeEnv.(extend empty) ("print", S (VarSet.empty, tarrow int_typ unit_typ))

let w e =
  Result.map (run (infer start_env e)) ~f:(fun (_, x) -> x)
  |> Result.map_error ~f:(function #error as x -> x)
;;

let%expect_test _ =
  let _ =
    let tv1 = tv 1 in
    let tv2 = tv 2 in
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
