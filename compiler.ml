(* load all the available plugins *)
let () = Sites.Plugins.Plugins.load_all ()

module Config = struct
  let dump_parsetree = ref false
  let dump_typedtree = ref false
  let out_file = ref "a.out"

  type input_from =
    | Stdin
    | File of string

  let input = ref Stdin
end

let default_args =
  [ "-dparsetree", Arg.Unit (fun () -> Config.dump_parsetree := true), " dump parsetree"
  ; "-dtypedtree", Arg.Unit (fun () -> Config.dump_typedtree := true), " dump typedtree"
  ; "-", Arg.Unit (fun () -> Config.input := Config.Stdin), "read from stdin"
  ]
;;

let () =
  let args = default_args @ !Registration.extra_cmd_switches in
  Arg.parse args (fun s -> Config.input := Config.File s) " MiniML compiler"
;;
