let extra_switches =
  [ ]


let compile _typed ~filename: _ = ()

let () =
  Registration.register_backend_exn "amd64" compile []
