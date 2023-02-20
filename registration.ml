type back = Miniml.Typedtree.structure -> filename:string -> unit

let current_backend : back option ref = ref None
let backends : (string * back) list ref = ref []
let extra_cmd_switches : (string * Arg.spec * string) list ref = ref []
let add_backend name back = backends := (name, back) :: !backends
let add_cmd_switches xs = extra_cmd_switches := !extra_cmd_switches @ xs

let register_backend_exn name back switches =
  try
    let (_ : back) = List.assoc name !backends in
    Printf.ksprintf failwith "Backend '%s' already registred" name
  with
  | Not_found ->
    add_backend name back;
    add_cmd_switches
      [ ( "-" ^ name
        , Arg.Unit (fun () -> current_backend := Some back)
        , Printf.sprintf " use %s backend" name )
      ];
    add_cmd_switches switches
;;
