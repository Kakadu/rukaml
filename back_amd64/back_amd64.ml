let extra_switches =
  [ ]


let compile _typed ~filename: _ = ()

let () =
  (* print_endline "Registration of Plugin1"; *)
  (* Queue.add (fun () -> print_endline "Plugin1 is doing something...") Registration.backends *)
  Registration.register_backend_exn "amd64" compile []
