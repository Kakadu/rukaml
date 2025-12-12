(* http://dev.stephendiehl.com/fun/006_hindley_milner.html *)

open Typedtree
open Base
module Format = Stdlib.Format (* silencing a warning *)

let use_logging = false
(* let use_logging = true *)

let log fmt =
  if use_logging
  then Format.kasprintf (fun s -> Format.printf "%s\n%!" s) fmt
  else Format.ifprintf Format.std_formatter fmt
;;

type error =
  [ `Occurs_check
  | `No_ident of Ident.t
  | `NoVariable of string
  | `UnificationFailed of ty * ty
  | `Only_varibles_on_the_left_of_letrec
  ]

let pp_error ppf : error -> _ = function
  | `Occurs_check -> Format.fprintf ppf "Occurs check failed"
  | `No_ident id -> Format.fprintf ppf "Undefined variable '%a'" Ident.pp id
  | `NoVariable s -> Format.fprintf ppf "Undefined variable '%s'" s
  | `UnificationFailed (l, r) ->
    Format.fprintf ppf "unification failed on %a and %a" Pprint.pp_typ l Pprint.pp_typ r
  | `Only_varibles_on_the_left_of_letrec ->
    Format.fprintf ppf "Only variables are allowed as left-hand side of `let rec'"
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

  val pp : Stdlib.Format.formatter -> t -> unit
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
      | TParam (a, t) -> tparam (helper a) t
      | TProd (a, b, ts) -> tprod (helper a) (helper b) (List.map ~f:helper ts)
      | TLink ty -> helper ty
      | Prim _ | Weak _ -> typ
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
      | Weak _ -> ()
      | Arrow (l, r) ->
        helper l;
        helper r
      | TParam (a, _) -> helper a
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
      | Prim _ | Weak _ -> acc
      | V { binder; _ } -> Var_set.add binder acc
      | TLink t -> helper acc t
      | TParam (a, _) -> helper acc a
      | Arrow (l, r) -> helper (helper acc l) r
      | TProd (a, b, ts) -> List.fold_left ts ~init:(helper (helper acc a) b) ~f:helper
    in
    helper Var_set.empty
  ;;

  let apply subs t = Subst.apply subs t
end

module Scheme = struct
  type t = scheme

  let make_mono ty = S (Var_set.empty, ty)

  (* let make_poly ty *)

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
  [%expect
    {|
    [ 1; ]
    [ 1; ] |}]
;;

module Type_env : sig
  type t = scheme Ident.Ident_map.t

  val pp : Format.formatter -> t -> unit
  val empty : t
  val extend : varname:string -> Ident.t -> scheme -> t -> t
  val extend_string : string -> scheme -> t -> t
  val extend_by_ident : Ident.t -> scheme -> t -> t
  val apply : Subst.t -> t -> t
  val ident_of_string_exn : string -> t -> Ident.t
  val find_exn : Ident.t -> t -> scheme
  val find_by_string_exn : string -> t -> scheme
  val free_vars : t -> Var_set.t
end = struct
  open Ident

  type t = scheme Ident.Ident_map.t

  let extend ~varname id scheme map = Ident.Ident_map.add varname id scheme map
  let extend_string varname = extend (Ident.of_string varname) ~varname

  let extend_by_ident ident scheme map =
    extend ~varname:ident.Ident.hum_name ident scheme map
  ;;

  let empty = Ident.Ident_map.empty
  let ident_of_string_exn = Ident.Ident_map.ident_of_string_exn

  let free_vars : t -> Var_set.t =
    Ident_map.fold_idents ~init:Var_set.empty ~f:(fun acc (_, s) ->
      Var_set.union acc (Scheme.free_vars s))
  ;;

  let apply s env = Ident_map.map env ~f:(Scheme.apply s)

  let pp ppf xs =
    Stdlib.Format.fprintf ppf "{| ";
    Ident_map.iter_idents xs ~f:(fun n s ->
      Stdlib.Format.fprintf ppf "%a -> %a; " Ident.pp n Pprint.pp_scheme s);
    Stdlib.Format.fprintf ppf "|}%!"
  ;;

  let find_exn = Ident.Ident_map.find_by_ident
  let find_by_string_exn = Ident_map.find_by_string_exn
end

open R
open R.Syntax

let unify weak l r =
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
    | TParam (a1, t1), TParam (a2, t2) ->
      (* TODO: make tests *)
      let* () = helper a1 a2 in
      if String.equal t1 t2 then return () else fail (`UnificationFailed (l, r))
    | TProd (a1, b1, ts1), TProd (a2, b2, ts2) ->
      let* () = helper a1 a2 in
      let* () = helper b1 b2 in
      if List.length ts1 = List.length ts2
      then
        List.fold2_exn ts1 ts2 ~init:(return ()) ~f:(fun acc l r ->
          let* () = acc in
          helper l r)
      else fail (`UnificationFailed (l, r))
    | Weak n, x ->
      weak.map <- IntMap.add n { typ_desc = x } weak.map;
      return ()
    | x, Weak n ->
      weak.map <- IntMap.add n { typ_desc = x } weak.map;
      return ()
    | TParam _, Arrow _
    | Arrow _, TParam _
    | TParam _, Prim _
    | Prim _, TParam _
    | TProd _, TParam _
    | TParam _, TProd _
    | TProd _, Arrow _
    | Arrow _, TProd _
    | TProd _, Prim _
    | Prim _, TProd _
    | Arrow _, Prim _
    | Prim _, Arrow _ -> fail (`UnificationFailed (l, r))
  in
  helper l r
;;

let instantiate ?(level = 0) : scheme -> ty R.t =
  fun (S (bs, t)) ->
  let rec next_name () =
    let* new_name = fresh in
    if Var_set.mem new_name bs then next_name () else return new_name
  in
  Var_set.fold_R
    (fun typ name ->
       let* f1 = next_name () in
       (* log "create fresh variable %d for name %d" f1 name; *)
       return @@ Subst.apply (Subst.singleton name (tv f1 ~level)) typ)
    bs
    (return t)
;;

let generalize : level:int -> Type.t -> Scheme.t =
  fun ~level ->
  let rec helper acc typ : binder_set =
    match typ.typ_desc with
    | V { var_level; binder } -> if var_level > level then Var_set.add binder acc else acc
    | TLink t -> helper acc t
    | TParam (a, _) -> helper acc a
    | Arrow (l, r) -> helper (helper acc l) r
    | TProd (a, b, tl) -> List.fold_left ~f:helper tl ~init:(helper (helper acc a) b)
    | Prim _ | Weak _ -> acc
  in
  fun ty ->
    (* log "generalize: @[%a@]" pp_ty ty; *)
    let free = helper Var_set.empty ty in
    (* let free = Var_set.diff (Type.free_vars ty) (Type_env.free_vars env) in *)
    S (free, ty)
;;

let lookup_env e xs =
  (* log "Looking up for %s" e;
     log "  inside %a" Type_env.pp xs; *)
  match List.Assoc.find_exn xs ~equal:Ident.equal e with
  | (exception Stdlib.Not_found) | (exception Not_found_s _) -> fail (`No_ident e)
  | scheme -> instantiate scheme
;;

let lookup_scheme : _ -> Type_env.t -> scheme t =
  fun id xs ->
  (* log "Looking up for %s" e;
     log "  inside %a" Type_env.pp xs; *)
  match Type_env.find_exn id xs with
  | (exception Stdlib.Not_found) | (exception Not_found_s _) -> fail (`No_ident id)
  | scheme -> return scheme
;;

let lookup_scheme_by_string : _ =
  fun s env ->
  match Type_env.find_by_string_exn s env with
  | scheme -> return scheme
  | (exception Stdlib.Not_found) | (exception Not_found_s _) -> fail (`NoVariable s)
;;

let fresh_var ~level = fresh >>| fun n -> tv n ~level

let pp_env subst ppf env =
  let env : Type_env.t =
    Ident.Ident_map.map ~f:(fun (S (args, v)) -> S (args, Subst.apply subst v)) env
  in
  Type_env.pp ppf env
;;

(** Introduce many fresh variables using in the for of a pattern *)
let rec check_pat ~level env : _ -> (_ * Typedtree.pattern * ty) t = function
  | Parsetree.PVar x ->
    let* tx = fresh_var ~level in
    let xident = Ident.of_string x in
    let env = Type_env.extend ~varname:x xident (Scheme.make_mono tx) env in
    return (env, Typedtree.Tpat_var xident, tx)
  | Parsetree.PTuple (p1, p2, []) ->
    let* env, p1, t1 = check_pat ~level env p1 in
    let* env, p2, t2 = check_pat ~level env p2 in
    return (env, Tpat_tuple (p1, p2, []), tprod t1 t2 [])
  | Parsetree.PTuple (_p1, _p2, _ps) -> failwith "Not implemented"
  | Parsetree.PAny -> failwith "TODO (psi) : not implemented"
  | Parsetree.PConstruct _ -> failwith "TODO (psi) : not implemented"
;;

let elim weak =
  let rec helper t =
    match t.typ_desc with
    | Prim _ | V _ -> t
    | Arrow (l, r) -> tarrow (helper l) (helper r)
    | TParam (a, t) -> tparam (helper a) t
    | TProd (a, b, tl) -> tprod (helper a) (helper b) (List.map tl ~f:helper)
    | TLink ty -> tlink (helper ty)
    | Weak n ->
      (try IntMap.find n weak.map with
       | Stdlib.Not_found -> tweak n)
  in
  helper
;;

let is_mutable = function
  | "array" (* | "ref" *) -> true
  | _ -> false
;;

type restriction_state =
  | MakeWeak (** A State, in which restrict makes weak types*)
  | DoNothing

type arrow_state =
  | OnTheRight (** covariant position *)
  | OnTheLeft (** contravariant position *)

let restrict : restriction_state -> weak_table -> ty -> ty t =
  fun start table t ->
  let rec helper { typ_desc } xs state arrow =
    match typ_desc with
    | V { binder; _ } ->
      (match state, arrow with
       | MakeWeak, OnTheRight | DoNothing, _ -> xs
       | MakeWeak, OnTheLeft ->
         if IntMap.mem binder xs
         then xs
         else (
           table.last <- table.last + 1;
           (* log "table.last counter number to binder %d: %d" binder table.last; *)
           IntMap.add binder table.last xs))
    | TLink t -> helper t xs state arrow
    | TParam (a, t) ->
      (* log "tparam: %s" t; *)
      let not_poly =
        match a.typ_desc with
        | V _ -> false
        | _ -> true
      in
      helper a xs (if is_mutable t && not_poly then MakeWeak else state) arrow
    | Prim _ | Weak _ -> xs
    | Arrow (l, r) ->
      (* log "%a" Pprint.pp_typ l; *)
      (* log "%a" Pprint.pp_typ r; *)
      let xs = helper r xs state OnTheRight in
      let xs = helper l xs state OnTheLeft in
      xs
    | TProd (f, s, ts) ->
      List.fold_left
        ts
        ~init:(helper f (helper s xs state arrow) state arrow)
        ~f:(fun acc x -> helper x acc state arrow)
  in
  let weak_map = helper t IntMap.empty start OnTheLeft in
  let rec helper t =
    match t.typ_desc with
    | Prim _ | Weak _ -> t
    | TLink l -> tlink (helper l)
    | V { binder; _ } ->
      (* log "introduce weak for binder: %d" binder; *)
      if IntMap.mem binder weak_map then tweak (IntMap.find binder weak_map) else t
    | TParam (a, t) -> tparam (helper a) t
    | Arrow (l, r) -> tarrow (helper l) (helper r)
    | TProd (a, b, ts) -> tprod (helper a) (helper b) (List.map ts ~f:helper)
  in
  return @@ helper t
;;

let infer env table expr =
  let current_level = ref 1 in
  let enter_level () =
    (* log "== enter level %d" (1 + !current_level); *)
    Int.incr current_level
  in
  let leave_level () =
    (* log "== leave level %d" !current_level; *)
    Int.decr current_level
  in
  let rec (helper
            : Type_env.t
              -> restriction_state
              -> Parsetree.expr
              -> (ty * Typedtree.expr) R.t)
    =
    fun env state -> function
      (* | Parsetree.EVar "=" -> *)
      (*  (\* TODO: make equality predefined *\) *)
      (*  let typ = tarrow int_typ (tarrow int_typ bool_typ) in *)
      (*  return (typ, TVar ("=", typ)) *)
      (*       |> extend_s *)
      (*      "get" *)
      (*      (Scheme.make_mono *)
      (*         (tarrow (array_typ @@ tv 1 ~level:1) (tarrow int_typ (tv 1 ~level:2)))) *)
      (* |> extend_s *)
      (*      "set" *)
      (*      (Scheme.make_mono *)
      (*         (tarrow *)
      (*            (array_typ @@ tv 1 ~level:1) *)
      (*            (tarrow int_typ (tarrow (tv 1 ~level:3) unit_typ)))) *)
      (* |> extend_s "length" (Scheme.make_mono (tarrow (array_typ @@ tv 1 ~level:1) int_typ)) *)
      | Parsetree.EVar "length" ->
        let* fresh = fresh_var ~level:!current_level in
        let typ = tarrow (tparam fresh "array") int_typ in
        return (typ, TVar ("length", Ident.of_string "length", typ))
      | Parsetree.EVar "get" ->
        let* fresh = fresh_var ~level:!current_level in
        let typ = tarrow (tparam fresh "array") (tarrow int_typ fresh) in
        return (typ, TVar ("get", Ident.of_string "get", typ))
      | Parsetree.EVar "set" ->
        let* fresh = fresh_var ~level:!current_level in
        let typ =
          tarrow (tparam fresh "array") (tarrow int_typ (tarrow fresh unit_typ))
        in
        return (typ, TVar ("set", Ident.of_string "set", typ))
      | Parsetree.EVar x ->
        let* scheme = lookup_scheme_by_string x env in
        let* typ = instantiate ~level:!current_level scheme in
        let typ = elim table typ in
        return (typ, TVar (x, Type_env.ident_of_string_exn x env, typ))
      | EUnit -> return (unit_typ, TUnit)
      | Parsetree.EArray r ->
        (match r with
         | [] ->
           let* ty = fresh_var ~level:!current_level in
           log "ty = %a" pp_ty ty;
           let ty = array_typ ty in
           return (ty, TArray ([], ty))
         | h :: _ ->
           let* ty, _ = helper env state h in
           let* _, exprs =
             list_foldm
               ~init:(return ([], []))
               ~f:(fun (typs, exprs) e ->
                 let* t1, e1 = helper env state e in
                 let* () = unify table ty t1 in
                 return (t1 :: typs, e1 :: exprs))
               r
           in
           let ty = elim table @@ array_typ ty in
           (* log "ty = %a" pp_ty ty; *)
           return (ty, TArray (exprs, ty)))
      (* lambda abstraction *)
      | ELam (PVar x, e1) ->
        let* tx = fresh_var ~level:!current_level in
        let xID = Ident.of_string x in
        let env = Type_env.extend ~varname:x xID (S (Var_set.empty, tx)) env in
        let* ty, tbody = helper env DoNothing e1 in
        (* log "ta = %a" pp_ty ta; *)
        let trez = elim table @@ tarrow tx ty in
        return (trez, TLam (Tpat_var xID, tbody, trez))
      | Parsetree.ELam ((PTuple _ as pat), body) ->
        let* env, pat, tp = check_pat ~level:!current_level env pat in
        let* ty, tbody = helper env DoNothing body in
        let trez = elim table @@ tarrow tp ty in
        return (trez, TLam (pat, tbody, trez))
      | EApp (e1, e2) ->
        let* t1, te1 = helper env state e1 in
        let* t2, te2 = helper env state e2 in
        let* tv = fresh_var ~level:0 in
        (* log "t1 = %a" pp_ty t1; *)
        (* log "t2 = %a" pp_ty t2; *)
        (* log "tv = %a" pp_ty tv; *)
        let* () = unify table t1 (tarrow t2 tv) in
        let* tv = restrict state table tv in
        let tv = elim table tv in
        (* log "t1 = %a" pp_ty t1; *)
        (* log "t2 = %a" pp_ty t2; *)
        (* log "tv = %a" pp_ty tv; *)
        return (tv, TApp (te1, te2, tv))
      | EConst (PConst_int _n as c) -> return (int_typ, TConst c)
      | EConst (PConst_bool _b as c) -> return (bool_typ, TConst c)
      | Parsetree.EIf (c, th, el) ->
        let* t1, tc = helper env state c in
        let* t2, tth = helper env state th in
        let* t3, tel = helper env state el in
        let* () = unify table t1 bool_typ in
        let* () = unify table t2 t3 in
        let t2 = elim table t2 in
        return (t2, TIf (tc, tth, tel, t2))
      | ETuple (a, b, es) ->
        let* ta, ea = helper env state a in
        let* tb, eb = helper env state b in
        let* typs, exprs =
          list_foldm
            ~init:(return ([], []))
            ~f:(fun (typs, exprs) e ->
              let* t1, e1 = helper env state e in
              return (t1 :: typs, e1 :: exprs))
            es
        in
        let tup_typ = elim table @@ tprod ta tb typs in
        return (tup_typ, TTuple (ea, eb, exprs, tup_typ))
      | Parsetree.ELet (NonRecursive, PVar x, rhs, e2) ->
        enter_level ();
        let* t1, typed_rhs = helper env state rhs in
        leave_level ();
        let t2 = generalize ~level:!current_level t1 in
        let x_ident = Ident.of_string x in
        let* t3, typed_in = helper (Type_env.extend ~varname:x x_ident t2 env) state e2 in
        let t3 = elim table t3 in
        return (t3, TLet (NonRecursive, Tpat_var x_ident, t2, typed_rhs, typed_in))
      | Parsetree.ELet (Recursive, PVar f, erhs, wher) ->
        let* tf = fresh_var ~level:!current_level in
        (* log "  var %s will have type %a (%d)" f Pprint.pp_typ tf Stdlib.__LINE__; *)
        enter_level ();
        let f_ident = Ident.of_string f in
        let* t1, typed_rhs =
          let env = Type_env.extend ~varname:f f_ident (S (Var_set.empty, tf)) env in
          helper env state erhs
        in
        leave_level ();
        let* () = unify table tf t1 in
        (* log "  var %s will have type %a" f Pprint.pp_typ tf; *)
        let t2 = generalize ~level:!current_level tf in
        (* log "letrec  result = %a\n%!" pp_schepme t2; *)
        let* twher, typed_wher =
          helper (Type_env.extend ~varname:f f_ident t2 env) state wher
        in
        let twher = elim table twher in
        return (twher, TLet (Recursive, Tpat_var f_ident, t2, typed_rhs, typed_wher))
      | ELet (Recursive, PTuple _, _, _) -> fail `Only_varibles_on_the_left_of_letrec
      | ELet (NonRecursive, (PTuple _ as pat), rhs, wher) ->
        let* env, pat, tp = check_pat ~level:!current_level env pat in
        let* _ty, tbody = helper env state rhs in
        let* () = unify table tp _ty in
        let* twher, typed_wher = helper env state wher in
        let twher = elim table twher in
        return (twher, TLet (NonRecursive, pat, Scheme.make_mono _ty, tbody, typed_wher))
      | Parsetree.ELet (_, PAny, _, _)
      | Parsetree.ELet (_, PConstruct _, _, _)
      | Parsetree.ELam (PAny, _)
      | Parsetree.ELam (PConstruct _, _)
      | Parsetree.EMatch _ | Parsetree.EConstruct _ ->
        failwith "TODO (psi) : not implemented"
  in
  let* ty, expr = helper env MakeWeak expr in
  let* ty = restrict DoNothing table ty in
  return (ty, expr)
;;

let start_env =
  let cmp_scheme = Scheme.make_mono (tarrow int_typ (tarrow int_typ bool_typ)) in
  let int_arith_scheme = Scheme.make_mono (tarrow int_typ (tarrow int_typ int_typ)) in
  let extend_s varname = Type_env.extend ~varname (Ident.of_string varname) in
  Type_env.empty
  |> extend_s "print" (Scheme.make_mono (tarrow int_typ unit_typ))
  |> extend_s "<" cmp_scheme
  |> extend_s ">" cmp_scheme
  |> extend_s "<=" cmp_scheme
  |> extend_s ">=" cmp_scheme
  |> extend_s "=" cmp_scheme
  |> extend_s "+" int_arith_scheme
  |> extend_s "-" int_arith_scheme
  |> extend_s "*" int_arith_scheme
  |> extend_s "/" int_arith_scheme
  |> extend_s "gc_compact" (Scheme.make_mono (tarrow unit_typ unit_typ))
  |> extend_s "gc_stats" (Scheme.make_mono (tarrow unit_typ unit_typ))
  |> extend_s "closure_count" (Scheme.make_mono (tarrow unit_typ unit_typ))
;;

let w e =
  let table = { map = IntMap.empty; last = 0 } in
  Result.map (run (infer start_env table e)) ~f:snd
  |> Result.map_error ~f:(function #error as x -> x)
;;

let vb ?(env = start_env) table (flg, pat, body) : (_, [> error ]) Result.t =
  let comp =
    match flg, pat with
    | Parsetree.NonRecursive, Parsetree.PVar name ->
      let* v = fresh in
      (* TODO: Why -1 is OK? *)
      let tv = Typedtree.tv v ~level:(-1) in
      let env = Type_env.extend_string name (S (Var_set.empty, tv)) env in
      let* ty, tbody = infer env table body in
      return (env, ty, Tpat_var (Type_env.ident_of_string_exn name env), tbody)
    | Recursive, PVar name ->
      let* v = fresh in
      let tv = Typedtree.tv v ~level:(-1) in
      let env = Type_env.extend_string name (S (Var_set.empty, tv)) env in
      let* ty, tbody = infer env table body in
      let* () = unify table tv (type_of_expr tbody) in
      return (env, ty, Tpat_var (Type_env.ident_of_string_exn name env), tbody)
    | Recursive, PTuple _ -> fail `Only_varibles_on_the_left_of_letrec
    | NonRecursive, PTuple _ -> failwith "Not implemented"
    | _ -> failwith "TODO (psi) : not implemented"
  in
  run comp
  |> Result.map ~f:(fun (env, ty, tpat, body) ->
    let vb = value_binding flg tpat body (generalize ~level:(-1) ty) in
    let env : Type_env.t =
      match pat, vb.Typedtree.tvb_pat with
      | Parsetree.PVar varname, Tpat_var vident ->
        Type_env.extend ~varname vident vb.Typedtree.tvb_typ env
      | _ -> failwith "Not implemented"
    in
    env, vb)
  |> Result.map_error ~f:(function #error as x -> x)
;;

let structure ?(env = start_env) table stru =
  let ( let* ) = Result.( >>= ) in
  let return = Result.return in
  let* _new_env, items =
    List.fold_left
      stru
      ~init:(return (env, []))
      ~f:(fun acc item ->
        match item with
        | Parsetree.SValue item ->
          let* env, acc = acc in
          let* env, new_item = vb ~env table item in
          return (env, new_item :: acc)
        | Parsetree.SType _ -> failwith "TODO (psi) : not implemented")
  in
  return (List.rev items)
;;

let%expect_test _ =
  let _ =
    let tv1 = tv 1 ~level:0 in
    let tv2 = tv 2 ~level:0 in
    let l = tarrow tv1 tv1 in
    let r = tarrow (tprim "int") tv2 in
    let weak = { map = IntMap.empty; last = 0 } in
    let subst = unify weak l r in
    let open Stdlib.Format in
    match R.run subst with
    | Result.Error _ -> ()
    | Ok () ->
      Format.printf " [ 1 -> %a, 2 -> %a ] %!" Pprint.pp_typ tv1 Pprint.pp_typ tv2
  in
  [%expect {| [ 1 -> int, 2 -> int ] |}]
;;
