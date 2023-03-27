let log_enables = ref false
let set_logging b = log_enables := b

let log fmt =
  if !log_enables
  then Format.kasprintf (Format.printf "%s\n%!") fmt
  else Format.ifprintf Format.std_formatter fmt
;;

let extra_switches = []

type error =
  [ Miniml.Inferencer.error
  | `Not_implemented
  ]

let pp_error ppf _ = Format.fprintf ppf "error?"

let codegen ppf _ =
  Format.fprintf ppf "; generated code for amd64\n%!";
  log "%s %d" __FILE__ __LINE__;
  Result.Ok ()
;;

let compile parsed _typed ~filename =
  let ( let* ) = Result.bind in
  let lifted_stru = CConv.(structure ~standart_globals) parsed in
  let* typed = Miniml.Inferencer.structure lifted_stru in
  let ch = Out_channel.open_text filename in
  let* () = codegen (Format.formatter_of_out_channel ch) typed in
  Out_channel.close ch;
  Result.ok ()
;;

let () =
  let module B = struct
    type nonrec error = error

    let pp_error = pp_error
    let run = compile
  end
  in
  Registration.register_backend_exn "amd64" (module B) []
;;
