let extra_switches = []
let compile _parsed _typed ~filename:_ = Result.Ok ()

type error

let pp_error ppf _ = Format.fprintf ppf "error?"

let () =
  let module B = struct
    type nonrec error = error

    let pp_error = pp_error
    let run = compile
  end
  in
  Registration.register_backend_exn "llvm" (module B) []
;;
