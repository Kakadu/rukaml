type config = Common.config

module Profile = Profile
module Devirt = Devirt
module Specialize = Specialize
module O2 = O2
module Tier0 = Tier0
module Online = Online

module type Pass = sig
  (** CLI name of the pass, as in [--opt=<name>]. *)
  val name : string

  (** Runtime symbols the pass makes the module call; the driver checks
      them against [BackEngine.runtime_symbols] before [load]. *)
  val requires : string list

  val run : IR.t -> IR.t
end

type pipeline = (module Pass) list

let empty : pipeline = []

let pass ~name ~requires ~run : (module Pass) =
  (module struct
    let name = name
    let requires = requires
    let run = run
  end)
;;

let make (cfg : config) : string -> (module Pass) option = function
  | "profile" ->
    Some (pass ~name:Profile.name ~requires:Profile.requires ~run:(Profile.run cfg))
  | "devirt" -> Some (pass ~name:Devirt.name ~requires:Devirt.requires ~run:Devirt.run)
  | "specialize" ->
    Some
      (pass ~name:Specialize.name ~requires:Specialize.requires ~run:(Specialize.run cfg))
  | "o2" -> Some (pass ~name:O2.name ~requires:O2.requires ~run:O2.run)
  | "tier0" -> Some (pass ~name:Tier0.name ~requires:Tier0.requires ~run:Tier0.run)
  | _ -> None
;;

let of_flags (cfg : config) (names : string list) : pipeline =
  let split_names =
    List.concat_map
      (fun s -> if String.contains s ',' then String.split_on_char ',' s else [ s ])
      names
  in
  List.map
    (fun n ->
       match make cfg n with
       | Some p -> p
       | None -> failwith (Printf.sprintf "unknown JIT optimization pass: %S" n))
    split_names
;;

let names (p : pipeline) = List.map (fun (module M : Pass) -> M.name) p
let requires (p : pipeline) = List.concat_map (fun (module M : Pass) -> M.requires) p

let missing_runtime ~available required =
  List.filter (fun sym -> not (List.mem sym available)) required
;;

(** [run ?dump pipeline ir] folds the passes left to right. After every
    pass the optional [dump] callback is invoked with the pass name and its
    output — the raw [IR.t], before the [; opts=...] header is prepended —
    so callers can observe every intermediate step. *)
let run ?(dump = fun (_ : string) (_ : IR.t) -> ()) (p : pipeline) (ir : IR.t) : IR.t =
  match p with
  | [] -> ir
  | passes ->
    let ir' =
      List.fold_left
        (fun acc (module M : Pass) ->
           let acc = M.run acc in
           dump M.name acc;
           acc)
        ir
        passes
    in
    let opts = String.concat "," (names p) in
    { ir' with
      IR.producer = ir'.IR.producer ^ "; opts=" ^ opts
    ; IR.payload = "; opts=" ^ opts ^ "\n" ^ IR.payload ir'
    }
;;
