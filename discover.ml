module C = Configurator.V1

let default_cflags = " -g -fPIC -Wall -Wpedantic"
let default_cc_rv64 = "riscv64-linux-gnu-gcc-13"
let default_cc_amd64 = "gcc-13"

(* TODO: print stuff on configure *)

let find_cc_rv64 cfg =
  let cc = Option.value (Sys.getenv_opt "CC_RV64") ~default:default_cc_rv64 in
  C.which cfg cc
;;

let find_cc_amd64 cfg =
  let cc = Option.value (Sys.getenv_opt "CC_AMD64") ~default:default_cc_amd64 in
  C.which cfg cc
;;

let () =
  C.main ~name:"rukaml" (fun cfg ->
    let cc_rv64 =
      find_cc_rv64 cfg |> Option.map (fun cc -> String.cat cc default_cflags)
    in
    let () =
      Out_channel.with_open_text "cc_rv64" (fun ch ->
        Option.value cc_rv64 ~default:"none" |> output_string ch)
    in
    let cc_amd64 =
      find_cc_amd64 cfg |> Option.map (fun cc -> String.cat cc default_cflags)
    in
    Out_channel.with_open_text "cc_amd64" (fun ch ->
      Option.value cc_amd64 ~default:"none" |> output_string ch))
;;
