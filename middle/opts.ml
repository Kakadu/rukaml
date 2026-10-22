open ANF
open Frontend

type opt_type = TailCall

type opt_state =
  | OptEnabled
  | OptLocallyDisabled (* for code regions, where opt is not allowed *)
  | OptGloballyDisabled
(* when user not passed "enable some optimization" to the compiler (e.g. -O2) *)

let opts : (opt_type, opt_state) Hashtbl.t = Hashtbl.create 5
let is_permitted opt = Hashtbl.find opts opt = OptEnabled

let init_opts ?(configured_opts = []) () =
  Hashtbl.add opts TailCall OptEnabled;
  List.iter (fun (k, v) -> Hashtbl.replace opts k v) configured_opts
;;

let disable_locally opt (f : unit -> 'a) : 'a =
  let state = Hashtbl.find opts opt in
  assert (state <> OptLocallyDisabled);
  if state = OptEnabled
  then (
    Hashtbl.replace opts opt OptLocallyDisabled;
    let res = f () in
    Hashtbl.replace opts opt OptEnabled;
    res)
  else f ()
;;

(* Now, only AMD64 backend uses this module.
   If we rewrite calling convention there like RV64,
   then we can remove this module
*)
module TailCall = struct
  let get_tailcalls expr pats =
    let rec parse_expr = function
      | ELet (_, _, _, wher) -> parse_expr wher
      | EComplex c_expr -> parse_c_expr c_expr
    and parse_c_expr = function
      | CApp (AVar f, _, _) -> Some [ f ]
      | CIte (_, bthn, bels) ->
        Some
          (List.concat_map
             (Option.value ~default:[])
             [ parse_expr bthn; parse_expr bels ])
      | _ -> None
    in
    let unfiltered = Option.value (parse_expr expr) ~default:[] in
    (* Filter to include only tailcalls of high-level functions *)
    let pats_names = List.filter_map
      (function 
       | Apat_var name -> Some name.Ident.hum_name
       | _ -> None) 
      pats 
    in
    List.filter (fun tc -> not (List.mem tc.Ident.hum_name pats_names)) unfiltered
  ;;

  let tailcalls : (Ident.t, Ident.t list) Hashtbl.t = Hashtbl.create 1024
  let funcs_stack_space : (Ident.t, int) Hashtbl.t = Hashtbl.create 1024

  let build_tailcalls_table vbs =
    let funcs =
      List.filter_map
        (function
          | `Function _ as f -> Some f
          | _ -> None)
        vbs
    in
    List.iter
      (fun (`Function (name, pats, expr)) ->
         assert (not (Hashtbl.mem tailcalls name));
         assert (not (Hashtbl.mem funcs_stack_space name));
         let maybe_tcs = get_tailcalls expr pats in
         Hashtbl.add tailcalls name maybe_tcs;
         Hashtbl.add funcs_stack_space name (List.length pats))
      funcs
  ;;

  let calculate_stack_space func =
    let rec calc visited func =
      let curr_space =
        Option.value (Hashtbl.find_opt funcs_stack_space func) ~default:0
      in
      if List.mem func visited
      then curr_space
      else (
        match Hashtbl.find_opt tailcalls func with
        | None -> curr_space
        | Some callees ->
          List.fold_left
            (fun s callee -> max s (calc (func :: visited) callee))
            curr_space
            callees)
    in
    calc [] func
  ;;

  let init_ss_table vbs =
    build_tailcalls_table vbs;
    let keys = Hashtbl.to_seq_keys funcs_stack_space in
    Seq.iter
      (fun key ->
         let space = calculate_stack_space key in
         let space_rounded = space + (space mod 2) in
         Hashtbl.replace funcs_stack_space key space_rounded)
      keys;
    (* Debug *)
      (* Hashtbl.iter (fun k v -> 
        print_endline @@ "Stack space of " ^ k.Ident.hum_name ^ " is " ^ string_of_int v; 
      ) funcs_stack_space;

      Hashtbl.iter (fun k ks -> 
        let names = List.map (fun x -> x.Ident.hum_name) ks in
        print_endline @@ "Tailcalls of " ^ k.Ident.hum_name ^ " are [" ^ (String.concat ", " names) ^ "]"; 
      ) tailcalls; *)
   
  ;;

  let get_stack_space name = Hashtbl.find funcs_stack_space (Option.get name)

  let get_free_space name busy =
    let all_space = get_stack_space name in
    assert (all_space >= busy);
    all_space - busy
  ;;

  (* For compability with AMD64 *)
  let vb_to_function = function
    | ANF_vb (_, Apat_var name, body) ->
      let pats, real_body = ANF.group_abstractions body in
      if List.length pats > 0 then Some (`Function (name, pats, real_body)) else None
    | _ -> None
  ;;
end
