(* https://gist.github.com/jozefg/652f1d7407b7f0266ae9 *)
module Format = Stdlib.Format
open Miniml

let log_enables = ref false
let set_logging b = log_enables := b

let log fmt =
  if !log_enables
  then Format.kasprintf (Format.printf "%s\n%!") fmt
  else Format.ifprintf Format.std_formatter fmt
;;

module String_set = struct
  include Set.Make (String)

  let pp ppf set =
    let open Format in
    fprintf ppf "{set| ";
    iter (fprintf ppf "%s, ") set;
    fprintf ppf "|set}"
  ;;
end

module SS = String_set

let simplify =
  let open Parsetree in
  let rec helper = function
    | ELam (PVar x, EApp ((ELam (_, _) as func), EVar y)) when x = y -> helper func
    | ELam (p, body) -> ELam (p, helper body)
    | (EVar _ as e) | (EConst _ as e) -> e
    | EIf (a, b, c) -> eite (helper a) (helper b) (helper c)
    | EApp (l, r) -> eapp (helper l) [ helper r ]
    | ETuple (a, b, es) -> etuple (helper a) (helper b) (List.map helper es)
    | ELet (isrec, pat, rhs, wher) -> elet ~isrec pat (helper rhs) (helper wher)
    | EUnit -> EUnit
  in
  helper
;;

let simplify_vb (flg, pat, rhs) = flg, pat, simplify rhs

let%expect_test " " =
  let ast = Parsing.parse_vb_exn "let ter loop = (fun n -> loop n 0) loop" in
  Format.printf "%a\n%!" Pprint.pp_value_binding (simplify_vb ast);
  [%expect {| let ter n = loop n 0 |}]
;;

let vars_from_pattern, vars_from_patterns =
  let rec helper acc = function
    | Parsetree.PVar name -> SS.add name acc
    | PTuple (a, b, ps) ->
      ListLabels.fold_left ~f:helper ~init:(helper (helper acc a) b) ps
  in
  helper SS.empty, List.fold_left helper SS.empty
;;

let free_vars_of_expr =
  let rec helper acc = function
    | Parsetree.EConst _ -> acc
    | EVar s -> String_set.add s acc
    | EIf (c, th, el) -> helper (helper (helper acc c) th) el
    | EApp (l, r) -> helper (helper acc l) r
    | ELet (NonRecursive, _, rhs, wher) -> helper (helper acc rhs) wher
    | ELet (Recursive, pat, rhs, wher) ->
      String_set.diff (helper (helper acc rhs) wher) (vars_from_pattern pat)
    | ETuple (a, b, es) -> List.fold_left helper (helper (helper acc a) b) es
    | ELam (pat, rhs) -> SS.diff (helper acc rhs) (vars_from_pattern pat)
    | EUnit -> acc
  in
  helper String_set.empty
;;

let is_free_in x term = String_set.mem x (free_vars_of_expr term)
let rec next_name s ~old = if String_set.mem s old then next_name ("_" ^ s) ~old else s

let rec subst x ~by:v =
  let open Parsetree in
  let rec helper e =
    match e with
    | EVar y when String.equal y x -> v
    | EVar y -> evar y
    | EApp (l, r) -> eapp (helper l) [ helper r ]
    | EConst _ -> e
    | ETuple (a, b, es) -> etuple (helper a) (helper b) (List.map helper es)
    | ELam (PVar y, b) when Stdlib.(y = x) -> elam (PVar y) b
    | ELam (PVar y, t) when is_free_in y v ->
      let frees = String_set.union (free_vars_of_expr v) (free_vars_of_expr t) in
      let w = next_name y ~old:frees in
      helper (elam (PVar w) (subst y ~by:(EVar w) t))
    | ELam (y, b) -> elam y (helper b)
    | EIf (a, b, c) -> EIf (helper a, helper b, helper c)
    | ELet (Recursive, PTuple _, _, _) ->
      failwith "Recursive tuples are not yet supported"
    | ELet ((NonRecursive as isrec), (PTuple _ as fpat), body, wher) ->
      let body = helper body in
      if SS.mem x (vars_from_pattern fpat)
      then elet ~isrec fpat body wher
      else elet ~isrec fpat body (helper wher)
    | ELet ((NonRecursive as isrec), (PVar fname as fpat), body, wher) ->
      let body = helper body in
      if fname = x
      then elet ~isrec fpat body wher
      else elet ~isrec fpat body (helper wher)
    | ELet ((Recursive as isrec), (PVar fname as fpat), body, wher) ->
      if fname = x
      then elet ~isrec fpat body wher
      else elet ~isrec fpat (helper body) (helper wher)
    | EUnit -> EUnit
  in
  helper
;;

let without set ~other = String_set.diff set other

let%expect_test " " =
  let left = String_set.of_list [ "a"; "b"; "c" ] in
  let other = String_set.of_list [ "d"; "b"; "c" ] in
  let set = without ~other left in
  Format.printf "%a\n" String_set.pp set;
  [%expect {|{set| a, |set} |}]
;;

let sugarize_let = Parsetree.group_lams

let gensym =
  let last = ref 0 in
  fun ?(prefix = "fresh") () ->
    incr last;
    Format.sprintf "%s_%d" prefix !last
;;

let standart_globals = String_set.of_list [ "+"; "="; "<"; "*"; "-" ]
let elams = List.fold_right Parsetree.elam

let conv ?(standart_globals = standart_globals)
  : Parsetree.value_binding -> Parsetree.value_binding list
  =
  let open Parsetree in
  let classify patterns expr =
    let fvs = free_vars_of_expr expr in
    log "free vars inside `%a` are:\n%a\n%!" Pprint.pp_expr expr SS.pp fvs;
    let names_in_pats =
      List.fold_left (fun acc p -> SS.union acc (vars_from_pattern p)) SS.empty patterns
    in
    let vars = without fvs ~other:(SS.union names_in_pats standart_globals) in
    if SS.cardinal vars = 0 then None else Some vars
  in
  let open Parsetree in
  (* TODO(Kakadu): don't know if monads are needed here *)
  let open Monads.Store in
  let save : value_binding -> (value_binding list, unit) t =
   fun x ->
    let* old = get in
    put (x :: old)
  in
  let is_abstraction = function
    | ELam _ -> true
    | _ -> false
  in
  let rec helper globals : Parsetree.expr -> (value_binding list, expr) t =
   fun root_expr ->
    log "Calling helper on @[%a@] and @[%a@]" SS.pp globals Pprint.pp_expr root_expr;
    match root_expr with
    | (EVar _ | EUnit | EConst _) as e -> return e
    | EIf (e1, e2, e3) ->
      return eite <*> helper globals e1 <*> helper globals e2 <*> helper globals e3
    (* | EApp (ELam (PVar arg, (ELam (_, _) as body)), EVar y) when arg = y ->
      (* fusion with simplifier *)
      helper globals body *)
    | EApp (l, r) -> return eapp1 <*> helper globals l <*> helper globals r
    | ELam (_, _) ->
      log "Got ELam _: globals = %a" SS.pp globals;
      (match sugarize_let root_expr with
       | arg_pats, rhs ->
         (match classify arg_pats rhs with
          | None ->
            (* TODO(Kakadu): Create a new lambda here too  *)
            log "None : %d" __LINE__;
            let new_f = gensym () in
            let* rhs = helper (SS.union (vars_from_patterns arg_pats) globals) rhs in
            let* () = save (NonRecursive, PVar new_f, elams arg_pats rhs) in
            return (EVar new_f)
          | Some extra ->
            log "Some %d, %a" __LINE__ SS.pp extra;
            let new_f = gensym () in
            (* TODO: maybe call on e too? *)
            let es = SS.to_seq extra |> List.of_seq in
            let* rhs = helper (SS.union (vars_from_patterns arg_pats) globals) rhs in
            let new_rhs =
              List.fold_left (fun acc x -> elam (PVar x) acc) (elams arg_pats rhs) es
            in
            let* () = save (NonRecursive, PVar new_f, new_rhs) in
            let new_call = eapp (EVar new_f) (List.rev_map evar es) in
            return new_call)
         (* let* e = helper globals e in
      return (elam (PVar v) e) *)
         (* let subj = ELam (PVar v, e) in
      log "\tgot expr : %a" Pprint.pp_expr subj;
      log "\tglobals  : %a" String_set.pp globals;
      let fvs = free_vars_of_expr e in
      log "conv.helper. fvs of rhs = %a" String_set.pp fvs;
      let vars = without fvs ~other:(String_set.add v globals) in
      log "conv.helper. vars = %a" String_set.pp vars;
      let ans = String_set.fold (fun name acc -> ELam (PVar name, acc)) vars subj in
      let ans = String_set.fold (fun name acc -> EApp (acc, EVar name)) vars ans in
      log "conv.helper. ans = %a" Pprint.pp_expr ans;
      return ans *))
    | ETuple (e1, e2, es) ->
      let* e1 = helper globals e1 in
      let* e2 = helper globals e2 in
      let* es = helper_list globals es in
      return (etuple e1 e2 es)
    | ELet (isrec, (PVar name as pat), rhs, wher) when is_abstraction rhs ->
      log "ELet %a" Pprint.pp_expr root_expr;
      let args, rhs =
        match sugarize_let rhs with
        | xs, rhs -> xs, rhs
      in
      let args_like =
        match isrec with
        | Recursive -> pat :: args
        | _ -> args
      in
      (match classify args_like rhs with
       | None ->
         log "classify says None";
         let new_env = String_set.union (vars_from_pattern pat) globals in
         let* rhs = helper new_env rhs in
         let* body = helper new_env wher in
         return (elet ~isrec pat rhs body)
       | Some extra ->
         log "classify says Some %a" String_set.pp extra;
         let* rhs = helper String_set.(union extra globals) rhs in
         let new_args = String_set.to_seq extra |> List.of_seq in
         (* TODO(Kakadu): replace multiple arguments by flat closures somewhere here *)
         let by =
           List.fold_right (fun name acc -> eapp1 acc (evar name)) new_args (evar name)
         in
         let rhs =
           match isrec with
           | Recursive ->
             let () =
               let open Pprint in
               log "Going to subst (inside a body) %a |~~> %a" pp_pattern pat pp_expr by
             in
             let rhs = subst name ~by rhs in
             let rhs = elams args rhs in
             let rhs =
               List.fold_left (fun acc name -> elam (PVar name) acc) rhs new_args
             in
             rhs
           | NonRecursive ->
             List.fold_left (fun acc name -> elam (PVar name) acc) rhs new_args
         in
         log "new rhs = %a" Pprint.pp_expr rhs;
         let () =
           let open Pprint in
           log "Going to subst (inside a wher) %a |~~> %a" pp_pattern pat pp_expr by
         in
         let wher = subst name ~by wher in
         log "next where = %a" Pprint.pp_expr wher;
         let* wher = helper (String_set.add name globals) wher in
         let* () = save (isrec, pat, rhs) in
         return wher)
    | ELet (Recursive, PTuple _, _, _) -> failwith "not implemented 2"
    | ELet (isrec, (PVar _name as pat), rhs, wher) ->
      assert (not (is_abstraction rhs));
      let new_env = String_set.union (vars_from_pattern pat) globals in
      let* rhs = helper new_env rhs in
      let* body = helper new_env wher in
      return (elet ~isrec pat rhs body)
    | ELet (NonRecursive, (PTuple _ as pat), rhs, wher) ->
      let new_env = String_set.union (vars_from_pattern pat) globals in
      let* rhs = helper new_env rhs in
      let* body = helper new_env wher in
      return (elet pat rhs body)
  and helper_list globals : Parsetree.expr list -> (value_binding list, expr list) t =
   fun es ->
    List.fold_left
      (fun acc e ->
        let* acc = acc in
        let* e = helper globals e in
        return (e :: acc))
      (return [])
      es
  in
  function
  | is_rec, (PVar v as pat), root ->
    let args, rhs = group_lams root in
    let enriched_globals = standart_globals |> String_set.add v in
    (* let enriched_globals =
      List.fold_left
        (fun acc -> function
          | PVar x -> String_set.add x acc
          | _ -> failwith "not implemented")
        enriched_globals
        args
    in *)
    let saved, last_rhs = Monads.Store.run (helper enriched_globals rhs) [] in
    List.rev_append saved [ is_rec, pat, elams args last_rhs ] |> List.map simplify_vb
  | is_rec, (PTuple _ as pat), root ->
    let saved, rhs =
      Monads.Store.run
        (helper (SS.union (vars_from_pattern pat) standart_globals) root)
        []
    in
    List.rev_append saved [ is_rec, pat, rhs ] |> List.map simplify_vb
;;

let value_binding = conv

let structure ?(standart_globals = standart_globals) stru =
  let init = standart_globals, [] in
  Stdlib.ListLabels.fold_left
    (stru : Parsetree.structure)
    ~init
    ~f:(fun (glob, ans) stru ->
      let new_strus = conv ~standart_globals:glob stru in
      let new_glob =
        ListLabels.fold_left ~init:glob new_strus ~f:(fun acc -> function
          | _, Parsetree.PVar s, _ -> String_set.add s acc
          | _, PTuple _, _ ->
            (* TODO(Kakadu): add other names too *)
            acc)
      in
      new_glob, List.append ans new_strus)
  |> snd
;;
