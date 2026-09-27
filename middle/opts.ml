open ANF
open Frontend

type opt_type = 
| TailCall

type opt_state = 
| OptEnabled 
| OptLocallyDisabled (* for code regions, where opt is not allowed *)
| OptGloballyDisabled (* when user not passed "enable some optimization" to the compiler (e.g. -O2) *) 

let opts : (opt_type, opt_state) Hashtbl.t = Hashtbl.create 5
let is_permitted opt = Hashtbl.find opts opt == OptEnabled

let init_opts ?(configured_opts=[]) () =
  Hashtbl.add opts TailCall OptEnabled;
  List.iter (fun (k, v) ->
    Hashtbl.replace opts k v
  ) configured_opts;
;;

let disable_locally opt (f : unit -> 'a) : 'a  = 
  let state = Hashtbl.find opts opt in 
  assert(state <> OptLocallyDisabled);
  if state == OptEnabled
  then(
    Hashtbl.replace opts opt OptLocallyDisabled;
    let res = f () in
    Hashtbl.replace opts opt OptEnabled;
    res
  )
  else f ()
;;

module TailCall = struct
  let get_tailcall expr = 
    let rec parse_expr = function
    | ELet (_, _, _, wher) -> parse_expr wher
    | EComplex c_expr -> parse_c_expr c_expr
    and parse_c_expr = function
    | CApp (AVar f, _, _) -> Some f
    | _ -> None
    in parse_expr expr
  ;;
  let tailcalls : (Ident.t, Ident.t) Hashtbl.t = Hashtbl.create 1024
  let funcs_stack_space: (Ident.t, int) Hashtbl.t = Hashtbl.create 1024

  let build_tailcalls_table vbs = 
    let funcs = List.filter_map (function 
    | `Function _ as f -> Some f
    | _ -> None) vbs in
    
    List.iter (fun (`Function (name, pats, expr)) ->
      assert(not (Hashtbl.mem tailcalls name));
      assert(not (Hashtbl.mem funcs_stack_space name));
      let maybe_tc = get_tailcall expr in
      Option.iter (fun callee -> Hashtbl.add tailcalls name callee) maybe_tc;
      Hashtbl.add funcs_stack_space name (List.length pats)
    ) funcs;
  ;;
  let init_ss_table vbs = 
    build_tailcalls_table vbs;
    let (keys, values) = (Hashtbl.to_seq_keys funcs_stack_space, Hashtbl.to_seq_values funcs_stack_space) in 
    let max_space = Seq.fold_left max 0 values in
    let max_space_rounded = max_space + (max_space mod 2) in
    Seq.iter (fun key -> Hashtbl.replace funcs_stack_space key max_space_rounded) keys;  
  ;;

  let get_stack_space name = Hashtbl.find funcs_stack_space (Option.get name)
  let get_free_space name busy = 
    let all_space = get_stack_space name in
    assert(all_space >= busy);
    all_space - busy
  ;;

  let vb_to_function = function 
  | ANF_vb (_, Apat_var name, body) ->
    let pats, _ = ANF.group_abstractions body in
    if List.length pats > 0
    then Some (`Function(name, pats, body) )
    else None
  | _ -> None
end