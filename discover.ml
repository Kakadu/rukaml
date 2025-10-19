open! Base
open Stdio

module C = Configurator.V1

type cmd =
  { path : string
  ; flags : string
  }

type defaults =
  { cc : cmd
  ; as_ : cmd
  ; ld : cmd
  ; run : cmd
  }

type toolchain =
  { compile : string option
  ; assemble : string option
  ; link : string option
  ; run : string option
  }
[@@deriving sexp_of]

let spf = Printf.sprintf

let log fmt = Printf.ksprintf (fun msg -> printf "discover: %s\n" msg) fmt

let discover_bin cfg path : string option =
  match C.which cfg path with
  | None ->
    log "%S is not available" path;
    None
  | Some _ -> Some path
;;

let discover_toolchain cfg defaults ~suffix =
  let discover_tool ~(env : cmd) ~(default : cmd) =
    Option.value (Sys.getenv env.path) ~default:default.path
    |> discover_bin cfg
    |> Option.map ~f:(fun bin ->
      let flags = Option.value (Sys.getenv env.flags) ~default:default.flags in
      String.concat ~sep:" " [ bin; flags ])
  in

  (* bind is used to make every next step run
     only if the previous one is successful *)
  let ( >>= ) = Option.( >>= ) in

  let suffix = String.uppercase suffix in
  let compile =
    log "discovering %s compiler" suffix;
    discover_tool
      ~env:{ path = spf "CC_%s" suffix; flags = spf "CFLAGS_%s" suffix }
      ~default:defaults.cc
  in

  let assemble =
    compile
    >>= fun _ ->
    log "discovering %s assembler" suffix;
    discover_tool
      ~env:{ path = spf "AS_%s" suffix; flags = spf "AS_FLAGS_%s" suffix }
      ~default:defaults.as_
  in

  let link =
    assemble
    >>= fun _ ->
    log "discovering %s linker" suffix;
    discover_tool
      ~env:{ path = spf "LD_%s" suffix; flags = spf "LD_FLAGS_%s" suffix }
      ~default:defaults.ld
  in

  let run =
    link
    >>= fun _ ->
    log "discovering %s runner" suffix;
    discover_tool
      ~env:{ path = spf "RUN_%s" suffix; flags = spf "RUN_FLAGS_%s" suffix }
      ~default:defaults.run
  in

  { compile; assemble; link; run }
;;

let export_toolchain toolchain ~suffix =
  let default = "none" in

  let compile = Option.value toolchain.compile ~default in
  Out_channel.write_all (spf "cc_%s" suffix) ~data:compile;

  let assemble = Option.value toolchain.assemble ~default in
  Out_channel.write_all (spf "as_%s" suffix) ~data:assemble;

  let link = Option.value toolchain.link ~default in
  Out_channel.write_all (spf "ld_%s" suffix) ~data:link;

  let run = Option.value toolchain.run ~default in
  Out_channel.write_all (spf "run_%s" suffix) ~data:run
;;

let () =
  let cfg = C.create "rukaml" in
  let print_toolchain toolchain =
    sexp_of_toolchain toolchain |> Sexp.to_string_hum ~indent:10 |> log "%s\n"
  in

  let gcc_amd64 = "gcc" in
  let defaults_amd64 =
    { cc = { path = gcc_amd64; flags = "-g -fPIC -Wall -Wpedantic" }
    ; as_ = { path = "nasm"; flags = "-f elf64" }
    ; ld = { path = gcc_amd64; flags = "" }
    ; run = { path = ""; flags = "" }
    }
  in
  let toolchain_amd64 = discover_toolchain cfg defaults_amd64 ~suffix:"amd64" in
  export_toolchain toolchain_amd64 ~suffix:"amd64";
  print_toolchain toolchain_amd64;

  let gcc_rv64 = "riscv64-unknown-linux-gnu-gcc" in
  let defaults_rv64 =
    { cc = { defaults_amd64.cc with path = gcc_rv64 }
    ; as_ = { path = gcc_rv64; flags = "-x assembler -c" }
    ; ld = { path = gcc_rv64; flags = "" }
    ; run = { path = "qemu-riscv64"; flags = "-L /opt/rv64/sysroot" }
    }
  in
  let toolchain_rv64 = discover_toolchain cfg defaults_rv64 ~suffix:"rv64" in
  export_toolchain toolchain_rv64 ~suffix:"rv64";
  print_toolchain toolchain_rv64;

  let clang = "clang" in
  let defaults_llvm =
    { cc = { defaults_amd64.cc with path = clang }
    ; as_ = { path = clang; flags = "-x ir -c" }
    ; ld = { path = clang; flags = "" }
    ; run = { path = ""; flags = "" }
    }
  in
  let toolchain_llvm = discover_toolchain cfg defaults_llvm ~suffix:"llvm" in
  export_toolchain toolchain_llvm ~suffix:"llvm";
  print_toolchain toolchain_llvm
;;
