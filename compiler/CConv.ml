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
    | ELet (isrec, pat, rhs, wher) -> elet ~isrec pat (helper rhs) (helper wher)
  in
  helper
;;

let simplify_vb (flg, pat, rhs) = flg, pat, simplify rhs

let%expect_test " " =
  let ast = Parsing.parse_vb_exn "let ter loop = (fun n -> loop n 0) loop" in
  Format.printf "%a\n%!" Pprint.pp_value_binding (simplify_vb ast);
  [%expect {| let ter n = loop n 0 |}]
;;

let free_vars_of_expr =
  let rec helper acc = function
    | Parsetree.EConst _ -> acc
    | EVar s -> String_set.add s acc
    | EIf (c, th, el) -> helper (helper (helper acc c) th) el
    | EApp (l, r) -> helper (helper acc l) r
    | ELet (NonRecursive, _, rhs, wher) -> helper (helper acc rhs) wher
    | ELet (Recursive, PVar pat, rhs, wher) ->
      String_set.remove pat (helper (helper acc rhs) wher)
    | ELam (Parsetree.PVar v, rhs) -> String_set.remove v (helper acc rhs)
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
    | ELam (PVar y, b) when Stdlib.(y = x) -> elam (PVar y) b
    | ELam (PVar y, t) when is_free_in y v ->
      let frees = String_set.union (free_vars_of_expr v) (free_vars_of_expr t) in
      let w = next_name y ~old:frees in
      helper (elam (PVar w) (subst y ~by:(EVar w) t))
    | ELam (y, b) -> elam y (helper b)
    | EConst _ -> e
    | EIf (a, b, c) -> EIf (helper a, helper b, helper c)
    | ELet ((NonRecursive as isrec), (PVar fname as fpat), body, wher) ->
      let body = helper body in
      if fname = x
      then elet ~isrec fpat body wher
      else elet ~isrec fpat body (helper wher)
    | ELet ((Recursive as isrec), (PVar fname as fpat), body, wher) ->
      if fname = x
      then elet ~isrec fpat body wher
      else elet ~isrec fpat (helper body) (helper wher)
  in
  helper
;;

let without set ~other = String_set.diff set other
(* String_set.fold (fun x acc -> String_set.remove x acc) other set *)

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
let elams names rhs = List.fold_right (fun x acc -> Parsetree.elam (PVar x) acc) names rhs

let conv ?(standart_globals = standart_globals)
  : Parsetree.value_binding -> Parsetree.value_binding list
  =
  let open Parsetree in
  let classify globals local_args expr =
    let fvs = free_vars_of_expr expr in
    log "free vars inside `%a` are:\n%a\n%!" Pprint.pp_expr expr SS.pp fvs;
    let vars = without fvs ~other:(String_set.union local_args globals) in
    if String_set.cardinal vars = 0 then None else Some vars
  in
  let open Parsetree in
  (* TODO(Kakadu): don't know if monads are needed here *)
  let open Monads.Store in
  let save : value_binding -> (value_binding list, unit) t =
   fun x ->
    let* old = get in
    put (x :: old)
  in
  let rec helper globals : Parsetree.expr -> (value_binding list, expr) t =
   fun root_expr ->
    log "Calling helper on @[%a@] and @[%a@]" SS.pp globals Pprint.pp_expr root_expr;
    match root_expr with
    | (EVar _ as e) | (EConst _ as e) -> return e
    | EIf (e1, e2, e3) ->
      return eite <*> helper globals e1 <*> helper globals e2 <*> helper globals e3
    (* | EApp (ELam (PVar arg, (ELam (_, _) as body)), EVar y) when arg = y ->
      (* fusion with simplifier *)
      helper globals body *)
    | EApp (l, r) -> return eapp1 <*> helper globals l <*> helper globals r
    | ELam (PVar _, _) ->
      (match sugarize_let root_expr with
       | args, rhs ->
         (match classify globals (SS.of_list args) rhs with
          | None ->
            log "None : %d" __LINE__;
            (* let new_f = gensym () in *)
            let* rhs = helper (SS.union (SS.of_list args) globals) rhs in
            (* let* () = save (NonRecursive, PVar new_f, elams args rhs) in *)
            (* return (EVar new_f) *)
            return (elams args rhs)
          | Some extra ->
            log "Some %d, %a" __LINE__ SS.pp extra;
            let new_f = gensym () in
            (* TODO: maybe call on e too? *)
            let es = SS.to_seq extra |> List.of_seq in
            let* rhs = helper (SS.union (SS.of_list args) globals) rhs in
            let new_rhs =
              List.fold_left (fun acc x -> elam (PVar x) acc) (elams args rhs) es
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
    | ELet (isrec, PVar pat, rhs, wher) ->
      let args, rhs =
        match sugarize_let rhs with
        | xs, rhs -> xs, rhs
      in
      let args_like =
        match isrec with
        | Recursive -> String_set.(of_list (pat :: args))
        | _ -> String_set.of_list args
      in
      (match classify globals args_like rhs with
       | None ->
         log "classify says None";
         let* rhs = helper (String_set.add pat globals) rhs in
         let* body = helper (String_set.add pat globals) wher in
         return (elet ~isrec (PVar pat) rhs body)
       | Some extra ->
         log "classify says Some %a" String_set.pp extra;
         let* rhs = helper String_set.(union extra globals) rhs in
         let new_args = String_set.to_seq extra |> List.of_seq in
         let by =
           List.fold_right (fun name acc -> eapp1 acc (evar name)) new_args (evar pat)
         in
         let rhs =
           match isrec with
           | Recursive ->
             let () =
               log "Going to subst (inside a body) %s |~~> %a" pat Pprint.pp_expr by
             in
             let rhs = subst pat ~by rhs in
             let rhs = List.fold_right (fun name acc -> elam (PVar name) acc) args rhs in
             let rhs =
               List.fold_left (fun acc name -> elam (PVar name) acc) rhs new_args
             in
             rhs
           | NonRecursive ->
             List.fold_left (fun acc name -> elam (PVar name) acc) rhs new_args
         in
         log "new rhs = %a" Pprint.pp_expr rhs;
         let () = log "Going to subst (inside a wher) %s |~~> %a" pat Pprint.pp_expr by in
         let wher = subst pat ~by wher in
         log "next where = %a" Pprint.pp_expr wher;
         let* wher = helper (String_set.add pat globals) wher in
         let* () = save (isrec, PVar pat, rhs) in
         return wher)
  in
  fun (is_rec, (PVar v as pat), root) ->
    let args, rhs = group_lams root in
    let saved, last_rhs =
      Monads.Store.run (helper (String_set.add v standart_globals) rhs) []
    in
    List.rev_append saved [ is_rec, pat, elams args last_rhs ] |> List.map simplify_vb
;;
