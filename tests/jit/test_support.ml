(* Shared runner and helpers for the JIT tests.

   Tests are independent named checks: a failing check is reported and the
   rest keep running, so one broken case does not hide the others. Successful
   checks are silent; the suite prints a single summary line at the end. *)

let tests = ref 0
let failures = ref 0
let fail fmt = Printf.ksprintf (fun msg -> failwith msg) fmt

let check ~name f =
  incr tests;
  match f () with
  | () -> ()
  | exception exn ->
    incr failures;
    print_endline ("FAIL " ^ name ^ ": " ^ Printexc.to_string exn)
;;

let expect name cond = check ~name (fun () -> if not cond then fail "%s" name)

let summary ~suite () =
  if !failures > 0
  then (
    Printf.eprintf "%s: %d/%d failed\n" suite !failures !tests;
    exit 1)
  else print_endline (Printf.sprintf "%s OK (%d tests passed)" suite !tests)
;;

let contains hay needle =
  let nh = String.length hay
  and nn = String.length needle in
  let rec loop i = i + nn <= nh && (String.sub hay i nn = needle || loop (i + 1)) in
  loop 0
;;

let raises_failure f =
  match f () with
  | _ -> false
  | exception Failure _ -> true
;;

let read_file path = In_channel.with_open_text path In_channel.input_all

let write_file path data =
  Out_channel.with_open_text path (fun oc -> Out_channel.output_string oc data)
;;

let with_tmp_file ?(suffix = "") contents f =
  let path = Filename.temp_file "rukaml_jit" suffix in
  write_file path contents;
  Fun.protect
    ~finally:(fun () -> if Sys.file_exists path then Sys.remove path)
    (fun () -> f path)
;;

let with_tmp_dir f =
  let path = Filename.temp_file "rukaml_jit" "" in
  Sys.remove path;
  Sys.mkdir path 0o700;
  Fun.protect
    ~finally:(fun () ->
      if Sys.file_exists path
      then (
        Array.iter
          (fun entry -> Sys.remove (Filename.concat path entry))
          (Sys.readdir path);
        Sys.rmdir path))
    (fun () -> f path)
;;

(* [dune test] runs from the build directory, [dune exec] from the workspace
   root; accept either location of the fixture directory. *)
let fixture_dir () =
  let near_exe = Filename.concat (Filename.dirname Sys.executable_name) "fixtures" in
  if Sys.file_exists near_exe then near_exe else "fixtures"
;;

let fixture name = IR.make IR.TextLL (read_file (Filename.concat (fixture_dir ()) name))
let ir_of payload = IR.make IR.TextLL payload
