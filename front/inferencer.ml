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
  | `Unbound_constructor of string
  | `Type_arity_mismatch of string
  | `Type_param_duplicates of string
  | `Unbound_type_variable of string
  | `Type_env_invariant_violation of string
  | `Unbound_type of string
  | `Constructor_arity_mismatch of string
  | `Constructor_name_duplicates of string
  | `InvalidFormatString of string
  ]

let pp_error ppf : error -> _ = function
  | `Occurs_check -> Format.fprintf ppf "Occurs check failed"
  | `No_ident id -> Format.fprintf ppf "Undefined variable '%a'" Ident.pp id
  | `NoVariable s -> Format.fprintf ppf "Undefined variable '%s'" s
  | `UnificationFailed (l, r) ->
    Format.fprintf ppf "unification failed on %a and %a" Pprint.pp_typ l Pprint.pp_typ r
  | `Only_varibles_on_the_left_of_letrec ->
    Format.fprintf ppf "Only variables are allowed as left-hand side of `let rec'"
  | `Unbound_constructor name -> Format.fprintf ppf "unbound constructror: %s" name
  | `Type_arity_mismatch name -> Format.fprintf ppf "type arity mismatch: %s" name
  | `Type_param_duplicates tyname ->
    Format.fprintf ppf "type parameter duplicates in the declaration of type %s" tyname
  | `Unbound_type_variable name -> Format.fprintf ppf "unbound type variable: %s" name
  | `Type_env_invariant_violation msg -> Format.fprintf ppf "%s" msg
  | `Unbound_type name -> Format.fprintf ppf "type %s was not declared" name
  | `Constructor_arity_mismatch name ->
    Format.fprintf ppf "constructor arity mistmatch: %s" name
  | `Constructor_name_duplicates name ->
    Format.fprintf ppf "constructor name duplicates in declaration of type %s" name
  | `InvalidFormatString fmt -> Format.fprintf ppf "\"%s\" is not a valid formatter" fmt
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
      | TProd (a, b, ts) -> tprod (helper a) (helper b) (List.map ~f:helper ts)
      | TLink ty -> helper ty
      | Weak _ -> typ
      | TConstr (tys, name) -> tconstr (List.map ~f:helper tys) name
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
      | TProd (a, b, ts) ->
        helper a;
        helper b;
        List.iter ts ~f:helper
      | TLink t -> helper t
      | TConstr (tys, _) -> List.iter tys ~f:helper
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
      | Weak _ -> acc
      | V { binder; _ } -> Var_set.add binder acc
      | TLink t -> helper acc t
      | Arrow (l, r) -> helper (helper acc l) r
      | TProd (a, b, ts) -> List.fold_left ts ~init:(helper (helper acc a) b) ~f:helper
      | TConstr (ts, _) -> List.fold_left ts ~init:acc ~f:helper
    in
    helper Var_set.empty
  ;;

  let apply subs t = Subst.apply subs t
end

let scheme vs ty = S (vs, ty)

module Scheme = struct
  type t = scheme

  let make_mono ty = S (Var_set.empty, ty)

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

module Type_env = struct
  include Typedtree.TypeEnv

  let extend ~varname ?(kind = User) id scheme t =
    { t with env_values = Ident.Ident_map.add varname id (scheme, kind) t.env_values }
  ;;

  let extend_string varname = extend (Ident.of_string varname) ~varname

  let extend_by_ident (ident : Ident.t) scheme t =
    extend ~varname:ident.hum_name ident scheme t
  ;;

  let ident_of_string s t = Ident.Ident_map.ident_of_string s t.env_values

  let free_vars t =
    Ident.Ident_map.fold_idents
      ~init:Var_set.empty
      ~f:(fun acc (_, (s, _)) -> Var_set.union acc (Scheme.free_vars s))
      t.env_values
  ;;

  let find_exn s t = Ident.Ident_map.find_by_ident s t.env_values
  let find_by_string s t = Ident.Ident_map.find_by_string s t.env_values

  let extend_constructors constr_info env =
    { env with
      env_constructors =
        Ident.String_map.add
          constr_info.constr_ident.hum_name
          constr_info
          env.env_constructors
    }
  ;;

  let extend_types td env =
    { env with
      env_types = Ident.Ident_map.add td.tty_ident.hum_name td.tty_ident td env.env_types
    }
  ;;

  let apply s env =
    let env_values =
      Ident.Ident_map.map env.env_values ~f:(fun (typ, info) -> Scheme.apply s typ, info)
    in
    { env with env_values }
  ;;
end

open R
open R.Syntax

let unify weak l r =
  let rec helper l r =
    match l.typ_desc, r.typ_desc with
    | TLink l, _ -> helper l r
    | _, TLink r -> helper l r
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
    | Weak n, x ->
      weak.map <- IntMap.add n { typ_desc = x } weak.map;
      return ()
    | x, Weak n ->
      weak.map <- IntMap.add n { typ_desc = x } weak.map;
      return ()
    | TConstr (tys1, name1), TConstr (tys2, name2)
      when String.equal name1 name2 && List.length tys1 = List.length tys2 ->
      List.fold2_exn
        ~f:(fun acc l r ->
          let* () = acc in
          helper l r)
        ~init:(return ())
        tys1
        tys2
    | _ -> fail (`UnificationFailed (l, r))
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
    | Arrow (l, r) -> helper (helper acc l) r
    | TProd (a, b, tl) -> List.fold_left ~f:helper tl ~init:(helper (helper acc a) b)
    | Weak _ -> acc
    | TConstr (tys, _) -> List.fold_left ~f:helper tys ~init:acc
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
  | scheme, _ -> return scheme
;;

let lookup_scheme_by_string : _ =
  fun s env ->
  match Type_env.find_by_string s env with
  | scheme -> return scheme
  | (exception Stdlib.Not_found) | (exception Not_found_s _) -> fail (`NoVariable s)
;;

let fresh_var ~level = fresh >>| fun n -> tv n ~level

let instantiate_tconstr ?(level = 0) name params =
  let* sub, vars =
    List.fold
      ~f:(fun acc binder ->
        let* sub, tvars = acc in
        let* fresh = fresh in
        let tvar = tv fresh ~level in
        let sub = Subst.compose sub (Subst.singleton binder tvar) in
        return (sub, tvar :: tvars))
      params
      ~init:(return (Subst.empty, []))
  in
  return (sub, tconstr (List.rev vars) name)
;;

let find_constructor name (env : Type_env.t) =
  match Ident.String_map.find_opt name env.env_constructors with
  | None -> fail (`Unbound_constructor name)
  | Some constr_entry ->
    let ty_ident = constr_entry.constr_type_ident in
    (match Ident.Ident_map.find_by_ident_opt ty_ident env.env_types with
     | None ->
       fail
         (`Type_env_invariant_violation "constructor references a non-existent type ident")
     | Some type_entry -> return (constr_entry, type_entry))
;;

let tpat_const_bool x = Tpat_const (PConst_bool x)
let tpat_const_unit = Tpat_unit

(** Introduce many fresh variables using in the for of a pattern *)
let rec check_pat ~level env table = function
  | Parsetree.PUnit -> return (env, Tpat_unit, unit_typ)
  | Parsetree.PConst (PConst_int n) -> return (env, Tpat_const (PConst_int n), int_typ)
  | Parsetree.PConst (PConst_bool b) -> return (env, Tpat_const (PConst_bool b), bool_typ)
  | Parsetree.PConst (PConst_char c) -> return (env, Tpat_const (PConst_char c), char_typ)
  | Parsetree.PConst (PConst_string s) ->
    return (env, Tpat_const (PConst_string s), string_typ)
  | Parsetree.PVar x ->
    let* tx = fresh_var ~level in
    let xident = Ident.of_string x in
    let env = Type_env.extend ~varname:x xident (Scheme.make_mono tx) env in
    return (env, Typedtree.Tpat_var xident, tx)
  | Parsetree.PTuple (p1, p2, ps) ->
    let check_many acc p =
      let* env, ps, ts = acc in
      let* env, p, t = check_pat ~level env table p in
      return (env, p :: ps, t :: ts)
    in
    let* env, p1, t1 = check_pat ~level env table p1 in
    let* env, p2, t2 = check_pat ~level env table p2 in
    let* env, ps, ts = List.fold ps ~init:(return (env, [], [])) ~f:check_many in
    return (env, Tpat_tuple (p1, p2, ps), tprod t1 t2 ts)
  | Parsetree.PAny ->
    let* ty = fresh_var ~level in
    return (env, Tpat_any, ty)
  | Parsetree.PConstruct (name, args) ->
    let* constr_info, type_info = find_constructor name env in
    let ty_name, ty_params = type_info.tty_ident.hum_name, type_info.tty_params in
    let* sub, ty = instantiate_tconstr ~level ty_name ty_params in
    let rec aux env args expected_tys acc_patts =
      match args, expected_tys with
      | [], [] ->
        return (env, Tpat_constr (constr_info.constr_ident, List.rev acc_patts), ty)
      | arg :: args, expected_ty :: expected_tys ->
        let* env, patt, arg_ty = check_pat ~level env table arg in
        let* () = unify table (Subst.apply sub expected_ty) (Subst.apply sub arg_ty) in
        aux env args expected_tys (patt :: acc_patts)
      | _ -> fail (`Constructor_arity_mismatch name)
    in
    (* delayed adt constructor arity calculating *)
    (match args, constr_info.constr_args with
     | ([ Parsetree.PTuple _ ] as actual), ty1 :: ty2 :: tys ->
       (* here the case of (ty1 * ... * tyN) is explicitly distinguished from the case of ty1 * ... * tyN *)
       let expected = [ { typ_desc = TProd (ty1, ty2, tys) } ] in
       aux env actual expected []
     | _ -> aux env args constr_info.constr_args [])
;;

let elim weak =
  let rec helper t =
    match t.typ_desc with
    | V _ -> t
    | Arrow (l, r) -> tarrow (helper l) (helper r)
    | TProd (a, b, tl) -> tprod (helper a) (helper b) (List.map tl ~f:helper)
    | TLink ty -> tlink (helper ty)
    | Weak n ->
      (try IntMap.find n weak.map with
       | Stdlib.Not_found -> tweak n)
    | TConstr (tys, name) -> tconstr (List.map tys ~f:helper) name
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
    | TConstr ([], _) -> xs
    | TConstr ([ param ], name) ->
      (* log "tparam: %s" ty; *)
      let not_poly =
        match param.typ_desc with
        | V _ -> false
        | _ -> true
      in
      helper param xs (if is_mutable name && not_poly then MakeWeak else state) arrow
    | TConstr (_many_params, name) when is_mutable name ->
      failwith "not implemented: restriction for many params mutable type constructors"
    | TConstr (params, _) ->
      List.fold_left params ~f:(fun acc x -> helper x acc state arrow) ~init:xs
    | Weak _ -> xs
    | Arrow (l, r) ->
      (* Check this branch: V^-(t1 -> t2) =  FTV(t1) U V^-(t2) *)

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
    | Weak _ -> t
    | TLink l -> tlink (helper l)
    | V { binder; _ } ->
      (* log "introduce weak for binder: %d" binder; *)
      if IntMap.mem binder weak_map then tweak (IntMap.find binder weak_map) else t
    | Arrow (l, r) -> tarrow (helper l) (helper r)
    | TProd (a, b, ts) -> tprod (helper a) (helper b) (List.map ts ~f:helper)
    | TConstr (tys, name) -> tconstr (List.map tys ~f:helper) name
  in
  return @@ helper t
;;

let infer_format3_of_string ~level s =
  let* out_ty = fresh_var ~level in
  let* dest_ty = fresh_var ~level in
  let rec helper chs acc_ty =
    (* TODO? it is probably faster then Angstrom monads for short format strings *)
    match chs with
    | [] -> return acc_ty
    | '%' :: 'd' :: tl -> helper tl (tarrow int_typ acc_ty)
    | '%' :: 'b' :: tl -> helper tl (tarrow bool_typ acc_ty)
    | '%' :: 'c' :: tl -> helper tl (tarrow char_typ acc_ty)
    | '%' :: 's' :: tl -> helper tl (tarrow string_typ acc_ty)
    | '%' :: 'a' :: tl ->
      let* fresh = fresh_var ~level in
      helper tl (tarrow (tarrow dest_ty (tarrow fresh acc_ty)) (tarrow fresh acc_ty))
    | '%' :: _ -> fail (`InvalidFormatString s)
    | _ :: tl -> helper tl acc_ty
  in
  let* arg_ty = helper (String.to_list s) out_ty in
  let ty = format3_typ ~arg_ty ~out_ty ~dest_ty in
  let expr = TFormat (s, ty) in
  return (ty, expr)
;;

let expects_format3 ty =
  match type_without_links ty with
  | { typ_desc = Arrow ({ typ_desc = TConstr (_, "format3") }, _) } -> true
  | _ -> false
;;

type string_inference_mode =
  | TypeString
  | TypeFormat3

type inferencer_state =
  { restriction_state : restriction_state
  ; infer_strings_as : string_inference_mode
  }

let clean_state { restriction_state; _ } =
  { restriction_state; infer_strings_as = TypeString }
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
              -> inferencer_state
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
      (* | Parsetree.EVar "length" ->
        let* fresh = fresh_var ~level:!current_level in
        let typ = tarrow (tparam fresh "array") int_typ in
        return (typ, TVar ("length", Ident.of_string "length", typ)) *)
      | Parsetree.EVar x ->
        let* scheme, kind = lookup_scheme_by_string x env in
        let* typ = instantiate ~level:!current_level scheme in
        let typ = elim table typ in
        return (typ, TVar (x, Type_env.ident_of_string x env, kind, typ))
      | EUnit -> return (unit_typ, TUnit)
      | Parsetree.EArray r ->
        let state = clean_state state in
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
      | Parsetree.ELam (pat, body) ->
        let state = clean_state state in
        let* env, pat, tp = check_pat ~level:!current_level env table pat in
        let* ty, tbody = helper env { state with restriction_state = DoNothing } body in
        let trez = elim table @@ tarrow tp ty in
        return (trez, TLam (pat, tbody, trez))
      | EApp (e1, e2) ->
        let state = clean_state state in
        let* t1, te1 = helper env state e1 in
        let* t2, te2 =
          helper
            env
            { state with
              infer_strings_as =
                (if t1 |> expects_format3 then TypeFormat3 else TypeString)
            }
            e2
        in
        let* tv = fresh_var ~level:0 in
        (* log "t1 = %a" pp_ty t1; *)
        (* log "t2 = %a" pp_ty t2; *)
        (* log "tv = %a" pp_ty tv; *)
        let* () = unify table t1 (tarrow t2 tv) in
        let* tv = restrict state.restriction_state table tv in
        let tv = elim table tv in
        (* log "t1 = %a" pp_ty t1; *)
        (* log "t2 = %a" pp_ty t2; *)
        (* log "tv = %a" pp_ty tv; *)
        return (tv, TApp (te1, te2, tv))
      | EConst (PConst_int _n as c) -> return (int_typ, TConst c)
      | EConst (PConst_char _c as c) -> return (char_typ, TConst c)
      | EConst (PConst_bool _b as c) -> return (bool_typ, TConst c)
      | EConst (PConst_string _s as c) ->
        (match state.infer_strings_as with
         | TypeString -> return (string_typ, TConst c)
         | TypeFormat3 -> infer_format3_of_string ~level:!current_level _s)
      | Parsetree.EIf (c, th, el) ->
        let* t1, tc = helper env (clean_state state) c in
        let* t2, tth = helper env state th in
        let* t3, tel = helper env state el in
        let* () = unify table t1 bool_typ in
        let* () = unify table t2 t3 in
        let t2 = elim table t2 in
        return (t2, TIf (tc, tth, tel, t2))
      | ETuple (a, b, es) ->
        let state = clean_state state in
        let* ta, ea = helper env state a in
        let* tb, eb = helper env state b in
        let* typs, exprs =
          list_foldm
            ~init:(return ([], []))
            ~f:(fun (typs, exprs) e ->
              let* ty, expr = helper env state e in
              return (ty :: typs, expr :: exprs))
            es
          >>| fun (tys, exprs) -> List.rev tys, List.rev exprs
        in
        let tup_typ = elim table @@ tprod ta tb typs in
        return (tup_typ, TTuple (ea, eb, exprs, tup_typ))
      | Parsetree.ELet (NonRecursive, PVar x, rhs, e2) ->
        enter_level ();
        let* t1, typed_rhs = helper env (clean_state state) rhs in
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
          helper env (clean_state state) erhs
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
      | ELet (Recursive, (PAny | PUnit | PConst _ | PTuple _ | PConstruct _), _, _) ->
        fail `Only_varibles_on_the_left_of_letrec
      | ELet (NonRecursive, lhs, rhs, wher) ->
        let* env, lhs, lhs_ty = check_pat ~level:!current_level env table lhs in
        let* rhs_ty, rhs = helper env (clean_state state) rhs in
        let* () = unify table lhs_ty rhs_ty in
        let* twher, typed_wher = helper env state wher in
        let twher = elim table twher in
        return (twher, TLet (NonRecursive, lhs, Scheme.make_mono rhs_ty, rhs, typed_wher))
      | EMatch (expr, ((p1, e1), cases)) ->
        let* expr_ty, expr = helper env (clean_state state) expr in
        let* env1, p1, pty = check_pat ~level:!current_level env table p1 in
        let* () = unify table pty expr_ty in
        let* ety, e1 = helper env1 state e1 in
        let infer_case acc (patt, expr) =
          let* pty_acc, ety_acc, cases = acc in
          let* env, patt, pty = check_pat ~level:!current_level env table patt in
          let* ety, expr = helper env state expr in
          let* () = unify table pty pty_acc in
          let* () = unify table ety ety_acc in
          let pty = elim table pty in
          let ety = elim table ety in
          return (pty, ety, (patt, expr) :: cases)
        in
        let* _pty, ety, cases =
          List.fold cases ~init:(return (pty, ety, [])) ~f:infer_case
        in
        return (ety, TMatch (expr, ((p1, e1), List.rev cases), ety))
      | EConstruct (name, args) ->
        let state = clean_state state in
        let* constr_info, type_info = find_constructor name env in
        let ty_name, ty_params = type_info.tty_ident.hum_name, type_info.tty_params in
        let* sub, ty = instantiate_tconstr ~level:!current_level ty_name ty_params in
        let rec aux args expected_tys acc_args =
          match args, expected_tys with
          | [], [] ->
            return (ty, TConstruct (constr_info.constr_ident, List.rev acc_args, ty))
          | arg :: args, expected_ty :: expected_tys ->
            let* arg_ty, arg_expr = helper env state arg in
            let* () =
              unify table (Subst.apply sub expected_ty) (Subst.apply sub arg_ty)
            in
            aux args expected_tys (arg_expr :: acc_args)
          | _ -> fail (`Constructor_arity_mismatch name)
        in
        (* delayed adt constructor arity calculating *)
        (match args, constr_info.constr_args with
         | arg1 :: arg2 :: args, ([ { typ_desc = TProd _ } ] as expected) ->
           (* here the case of (ty1 * ... * tyN) is explicitly distinguished from the case of ty1 * ... * tyN *)
           let actual = [ Parsetree.ETuple (arg1, arg2, args) ] in
           aux actual expected []
         | _ -> aux args constr_info.constr_args [])
  in
  let init_state = { restriction_state = MakeWeak; infer_strings_as = TypeString } in
  let* ty, expr = helper env init_state expr in
  let* ty = restrict DoNothing table ty in
  return (ty, expr)
;;

let ( @-> ) = tarrow

let start_env =
  let cmp_scheme = Scheme.make_mono (tarrow int_typ (tarrow int_typ bool_typ)) in
  let int_arith_scheme = Scheme.make_mono (tarrow int_typ (tarrow int_typ int_typ)) in
  let bool_arith_scheme = Scheme.make_mono (tarrow bool_typ (tarrow bool_typ bool_typ)) in
  let extend_s ?(kind = User) varname =
    Type_env.extend ~varname ~kind (Ident.of_string varname)
  in
  let extend_binop name = extend_s ~kind:(Builtin (name, 2)) name in
  Type_env.env_with_base_types
  (* TODO: print_int *)
  |> extend_s
       "print"
       (Scheme.make_mono (tarrow int_typ unit_typ))
       ~kind:(Builtin ("print", 1))
  |> extend_s
       "char_code"
       (Scheme.make_mono (tarrow char_typ int_typ))
       ~kind:(Builtin ("char_code", 1))
     (* ***** StdIO stuff *)
  |> extend_s "stdin" (Scheme.make_mono (array_typ char_typ))
  |> extend_s
       "open_in"
       (Scheme.make_mono (tarrow (array_typ char_typ) (array_typ char_typ)))
  (* Stdio channels *)
  |> extend_s "stdin" (Scheme.make_mono in_channel_typ)
  |> extend_s "stdout" (Scheme.make_mono out_channel_typ)
  |> extend_s "stderr" (Scheme.make_mono out_channel_typ)
  (* Stdio file access primitives *)
  |> extend_s "open_in" (Scheme.make_mono (tarrow string_typ in_channel_typ))
  |> extend_s "open_out" (Scheme.make_mono (tarrow string_typ out_channel_typ))
  |> extend_s "close_in" (Scheme.make_mono (tarrow in_channel_typ unit_typ))
  |> extend_s "close_out" (Scheme.make_mono (tarrow out_channel_typ unit_typ))
  (* Stdio reading primitives *)
  |> extend_s "input_char" (Scheme.make_mono (tarrow in_channel_typ char_typ))
  |> extend_s "input_line" (Scheme.make_mono (tarrow in_channel_typ string_typ))
  |> extend_s "end_of_file" (Scheme.make_mono (tarrow in_channel_typ bool_typ))
  (* Stdio writing primitives *)
  |> extend_s
       "output_char"
       (Scheme.make_mono (tarrow out_channel_typ (tarrow char_typ unit_typ)))
  |> extend_s
       "output_string"
       (Scheme.make_mono (tarrow out_channel_typ (tarrow string_typ unit_typ)))
  |> extend_s
       "sprintf"
       (let arg_ty = tv 0 ~level:(-1) in
        (* forall '_0 . ('_0, unit, string) format3 -> '_0 *)
        scheme
          (Var_set.singleton 0)
          (tarrow (format3_typ ~arg_ty ~dest_ty:unit_typ ~out_ty:string_typ) arg_ty))
  |> extend_s
       "fprintf"
       (let arg_ty = tv 0 ~level:(-1) in
        (* forall '_0 . out_channel -> ('_0, out_channel, unit) format3 -> '_0 *)
        scheme
          (Var_set.singleton 0)
          (tarrow
             out_channel_typ
             (tarrow
                (format3_typ ~arg_ty ~dest_ty:out_channel_typ ~out_ty:unit_typ)
                arg_ty)))
  |> extend_s
       "printf"
       (let arg_ty = tv 0 ~level:(-1) in
        (* forall '_0 . ('_0, out_channel, unit) format -> 'a *)
        scheme
          (Var_set.singleton 0)
          (tarrow (format3_typ ~arg_ty ~dest_ty:out_channel_typ ~out_ty:unit_typ) arg_ty))
  |> extend_s "flush" (Scheme.make_mono (tarrow out_channel_typ unit_typ))
  (* Built-in binops *)
  |> extend_binop "<" cmp_scheme
  |> extend_binop ">" cmp_scheme
  |> extend_binop "<=" cmp_scheme
  |> extend_binop ">=" cmp_scheme
  |> extend_binop "=" cmp_scheme
  |> extend_binop "+" int_arith_scheme
  |> extend_binop "-" int_arith_scheme
  |> extend_binop "*" int_arith_scheme
  |> extend_binop "/" int_arith_scheme
  |> extend_binop "&&" bool_arith_scheme
  |> extend_binop "||" bool_arith_scheme
  (* Array stuff *)
  |> extend_s
       "length"
       (Typedtree.S (Var_set.singleton 0, tarrow (array_typ (tv 0 ~level:1000)) int_typ))
  |> extend_s
       "get"
       (let param = tv 0 ~level:1000 in
        let ( @-> ) = tarrow in
        Typedtree.S (Var_set.singleton 0, array_typ param @-> int_typ @-> param))
  |> extend_s
       "set"
       (let param = tv 0 ~level:1000 in
        Typedtree.S
          (Var_set.singleton 0, array_typ param @-> int_typ @-> param @-> unit_typ))
  (* strings stuff *)
  |> extend_s "string_len" (Scheme.make_mono (tarrow string_typ int_typ))
  |> extend_s
       "string_nth"
       (Scheme.make_mono (tarrow int_typ (tarrow string_typ char_typ)))
  (* GC stuff *)
  |> extend_s "gc_compact" (Scheme.make_mono (tarrow unit_typ unit_typ))
  |> extend_s "gc_stats" (Scheme.make_mono (tarrow unit_typ unit_typ))
  |> extend_s "closure_count" (Scheme.make_mono (tarrow unit_typ unit_typ))
  |> extend_s
       "trace_rukaml_val"
       (S (Var_set.singleton 0, tarrow (tv ~level:1000 0) unit_typ))
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
      return (env, ty, Tpat_var (Type_env.ident_of_string name env), tbody)
    | Recursive, PVar name ->
      let* v = fresh in
      let tv = Typedtree.tv v ~level:(-1) in
      let env = Type_env.extend_string name (S (Var_set.empty, tv)) env in
      let* ty, tbody = infer env table body in
      let* () = unify table tv (type_of_expr tbody) in
      return (env, ty, Tpat_var (Type_env.ident_of_string name env), tbody)
    | Recursive, PTuple _ -> fail `Only_varibles_on_the_left_of_letrec
    | NonRecursive, PTuple _ -> failwith "Not implemented"
    | _ -> failwith "not implemented"
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

let ( <*> ) mf mx = mf >>= fun f -> mx >>= fun x -> return (f x)

let check_constr_arity (env : Type_env.t) name args =
  match Ident.Ident_map.find_by_string_opt name env.env_types with
  | None -> fail (`Unbound_type name)
  | Some type_declaration ->
    if List.length args <> List.length type_declaration.tty_params
    then fail (`Type_arity_mismatch name)
    else return ()
;;

let rec infer_core_type (env : Type_env.t) param_map = function
  | Parsetree.Ptyp_var name ->
    (match Ident.String_map.find_opt name param_map with
     | None -> fail (`Unbound_type_variable name)
     | Some ty -> return ty)
  | Ptyp_arrow (a, b) ->
    return (fun a b -> tarrow a b)
    <*> infer_core_type env param_map a
    <*> infer_core_type env param_map b
  | Ptyp_tuple (a, b, xs) ->
    return (fun a b xs -> tprod a b xs)
    <*> infer_core_type env param_map a
    <*> infer_core_type env param_map b
    <*> fold_infer_core_type env param_map xs
  | Ptyp_constr (name, args) ->
    let* () = check_constr_arity env name args in
    let* tys = fold_infer_core_type env param_map args in
    return (tconstr tys name)

and fold_infer_core_type (env : Type_env.t) param_map items =
  List.fold (List.rev items) ~init:(return []) ~f:(fun acc item ->
    let* acc = acc in
    let* ty = infer_core_type env param_map item in
    return (ty :: acc))
;;

let check_params_uniqueness pty_name pty_params =
  if
    List.length (List.dedup_and_sort pty_params ~compare:String.compare)
    <> List.length pty_params
  then fail (`Type_param_duplicates pty_name)
  else return ()
;;

let check_constructors_names_uniqueness pty_name variants =
  let names = List.map ~f:(fun (name, _) -> name) variants in
  if List.length (List.dedup_and_sort names ~compare:String.compare) <> List.length names
  then fail (`Constructor_name_duplicates pty_name)
  else return ()
;;

let td ?(env = start_env) { Parsetree.pty_name; pty_params; pty_kind; pty_manifest }
  : (_, [> error ]) Result.t
  =
  let init_params params_list =
    let helper acc name =
      let* map, bs = acc in
      let* binder = fresh in
      let ty = tv binder ~level:(-1) in
      let map = Ident.String_map.add name ty map in
      return (map, binder :: bs)
    in
    List.fold ~init:(return (Ident.String_map.empty, [])) ~f:helper params_list
    >>| fun (map, params) -> map, List.rev params
  in
  let comp =
    let* () = check_params_uniqueness pty_name pty_params in
    let* params_map, tty_params = init_params pty_params in
    let tty_ident = Ident.of_string pty_name in
    let* tty_manifest =
      match pty_manifest with
      | None -> return None
      | Some core_type -> infer_core_type env params_map core_type >>| Option.some
    in
    match pty_kind with
    | Parsetree.Ptype_variant (v1, vs) ->
      let* () = check_constructors_names_uniqueness pty_name (v1 :: vs) in
      (* > temporarily adds type to env to use it in recursive type declarations *)
      let env =
        Type_env.extend_types
          { tty_kind = Ttype_abstract; tty_ident; tty_params; tty_manifest }
          env
      in
      (* < *)
      let aux acc (name, args) =
        let* env, variants, id_cnt = acc in
        let* constr_args = fold_infer_core_type env params_map args in
        let constr_ident = Ident.ident name id_cnt in
        let constr_info = { constr_ident; constr_type_ident = tty_ident; constr_args } in
        let env = Type_env.extend_constructors constr_info env in
        return (env, constr_info :: variants, id_cnt + 1)
      in
      let* env, variants, _ = List.fold ~init:(return (env, [], 0)) ~f:aux (v1 :: vs) in
      let tty_kind = Ttype_variants (List.rev variants) in
      let td = { tty_kind; tty_ident; tty_params; tty_manifest = None } in
      let env = Type_env.extend_types td env in
      return (env, td)
    | Parsetree.Ptype_abstract ->
      let td = { tty_kind = Ttype_abstract; tty_ident; tty_params; tty_manifest } in
      let env = Type_env.extend_types td env in
      return (env, td)
  in
  run comp
;;

let structure_item ?(env = start_env) table pstru_item =
  let ( let* ) = Result.( >>= ) in
  let return = Result.return in
  match pstru_item with
  | Parsetree.Pstr_value item ->
    let* env, typed_vb = vb ~env table item in
    return (env, Tstr_value typed_vb)
  | Parsetree.Pstr_type (item, []) ->
    let* env, stru_item = td ~env item in
    return (env, Tstr_type stru_item)
  | _ -> failwith "not implemented: \"and\" chain of type declarations"
;;

let structure ?(env = start_env) table stru =
  let ( let* ) = Result.( >>= ) in
  let return = Result.return in
  let* env, items =
    List.fold_left
      stru
      ~init:(return (env, []))
      ~f:(fun acc item ->
        let* env, items = acc in
        let* new_env, new_item = structure_item ~env table item in
        return (new_env, new_item :: items))
  in
  return (env, List.rev items)
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
