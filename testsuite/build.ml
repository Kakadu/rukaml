open! Base
open Stdio

module Path = File_path.Relative
module PPart = File_path.Part

let spf = Printf.sprintf

(** Compilation target *)
module Target = struct
  type name =
    | Amd64
    | Rv64
  [@@deriving sexp, show { with_path = false }]

  type t =
    { name : name
    ; assemble_cmd : Path.t
    ; link_cmd : Path.t
    ; run_cmd : Path.t
    }

  let show_name = Fn.compose String.lowercase show_name

  (** Artifact compiled/linked/assembled etc. for the target *)
  type _ art =
    { name : string
    ; rule : string
    }

  (** Artifact with path of the directory its stored in attached *)
  type 'a art_located =
    { dir : Path.t
    ; art : 'a art
    }

  let compile_rule =
    Stdlib.format_of_string
      {|
(rule
 (target %s)
 (deps %s)
 (mode promote)
 (action
  (run %%{project_root}/back/%s/%s_compiler.exe %%{deps} -o %%{target})))
|}
  ;;

  (** Compile input file to target's IL.
      Artifact's name is derived from input path - prefix path *)
  let compile (tgt : t) ~(input : Path.t) ~(prefix : Path.t) : [ `Compile ] art =
    let tgt_name = show_name tgt.name in
    let name =
      Path.chop_prefix_if_exists input ~prefix
      |> Path.to_parts
      |> List.map ~f:PPart.to_string
      |> String.concat ~sep:"."
      |> String.chop_suffix_if_exists ~suffix:".ml"
      |> fun x -> String.concat [ x; "."; tgt_name; ".out" ]
    in
    let rule = spf compile_rule name (Path.to_string input) tgt_name tgt_name in
    { name; rule }
  ;;

  let assemble_rule =
    Stdlib.format_of_string
      {|
(rule
  (target %s)
  (deps %s)
  (mode (promote (until-clean) (into obj)))
  (enabled_if (not (= none %%{read:%s})))
  (action (system "%%{read:%s} %%{deps} -o %%{target}")))
|}
  ;;

  (** Assemble target's IL to arch-specific object file *)
  let assemble tgt (input : [ `Compile ] art_located) : [ `Assemble ] art =
    let name = String.chop_suffix_if_exists input.art.name ~suffix:".out" ^ ".o" in
    let input = Path.append_part input.dir (PPart.of_string input.art.name) in
    let cmd = Path.to_string tgt.assemble_cmd in
    let rule = spf assemble_rule name (Path.to_string input) cmd cmd in
    { name; rule }
  ;;

  let link_rule =
    Stdlib.format_of_string
      {|
(rule
  (target %s)
  (deps %s %s)
  (mode (promote (until-clean) (into exe)))
  (enabled_if (not (= none %%{read:%s})))
  (action (system "%%{read:%s} %%{deps} -o %%{target}")))
|}
  ;;

  (** Link object file with runtime to produce executable *)
  let link tgt (input : [ `Assemble ] art_located) ~(runtime : Path.t) : [ `Link ] art =
    let name = String.chop_suffix_if_exists input.art.name ~suffix:".o" ^ ".exe" in
    let input = Path.append_part input.dir (PPart.of_string input.art.name) in
    let cmd = Path.to_string tgt.link_cmd in
    let rule =
      spf link_rule name (Path.to_string runtime) (Path.to_string input) cmd cmd
    in
    { name; rule }
  ;;

  let run_rule =
    Stdlib.format_of_string
      {|
(rule
  (mode (promote (until-clean) (into cram)))
  (enabled_if (not (= none %%{read:%s})))
  (action (with-stdout-to %s
    (echo "  $ %%{read:%s} %s\n  %s\n%s"))))
|}
  ;;

  (** Run (or rather generate cram test to run) executable
      with expected stdout and exit code *)
  let run tgt (input : [ `Link ] art_located) ~(stdout : string list) ~(exit : int)
    : [ `Run ] art
    =
    let name = String.chop_suffix_if_exists input.art.name ~suffix:".exe" ^ ".t" in
    let input = Path.append_part input.dir (PPart.of_string input.art.name) in
    let cmd = Path.to_string tgt.run_cmd in
    let stdout = String.concat ~sep:"\n  " stdout in
    let exit = if exit = 0 then "" else spf "  [%d]\n" exit in
    let rule = spf run_rule cmd name cmd (Path.to_string input) stdout exit in
    { name; rule }
  ;;
end

(** Test specification parsed from user comments *)
module TestSpec = struct
  (* Records are specifically avoided in spec because
     deriving sexp produces too many brackets on them.
     Sum types look cleaner in comparison though require
     all the parameters to be present in specific order.

     XXX: Move from deriving sexp to custom parser *)

  type targets = Targets of Target.name list [@sexp.list] [@@deriving sexp]

  type stdout = Stdout of string list [@sexp.list] [@@deriving sexp]
  type exit = Exit of int [@@deriving sexp]
  type run_spec =
    | Norun
    | Run of exit * stdout
  [@@deriving sexp]

  type t = Test of targets * run_spec [@@deriving sexp]

  let of_file (path : Path.t) =
    let ( let* ), return, fail = Result.( >>= ), Result.return, Result.fail in
    let rec parse_next parse file =
      let* line = In_channel.input_line file |> Result.of_option ~error:"eof" in
      match parse (`String line) with
      | Angstrom.Buffered.Done (_, res) -> return res
      | Fail (_, _, msg) -> fail msg
      | Partial f -> parse_next f file
    in

    let pcomment =
      let open Angstrom in
      skip_while Char.is_whitespace *> string "(*" *> many_till any_char (string "*)")
      >>| String.of_char_list
    in

    let parse = Angstrom.Buffered.feed @@ Angstrom.Buffered.parse pcomment in
    let* comment = In_channel.with_file (Path.to_string path) ~f:(parse_next parse) in

    let* sexp =
      match Parsexp.Many.parse_string comment with
      | Error err -> fail (Parsexp.Parse_error.message err)
      | Ok [] -> fail "test spec not found"
      | Ok list -> return (Sexp.List list)
    in
    try return (t_of_sexp sexp) with
    | Sexplib.Conv_error.Of_sexp_error (Failure msg, _) -> fail msg
  ;;
end

(** Compiler's test case *)
module Test = struct
  type t =
    { path : Path.t
    ; spec : TestSpec.t
    }

  type dune_rules =
    { expected : string list
    ; artifacts : string list
    ; cram : string list
    }

  let rules_empty = { expected = []; artifacts = []; cram = [] }

  let target_of_name tgt_name ~root =
    let name = Target.show_name tgt_name in
    let cmd ~prefix =
      Path.append_part (Path.append Path.dot_dot root) @@ PPart.of_string (prefix ^ name)
    in
    Target.
      { name = tgt_name
      ; assemble_cmd = cmd ~prefix:"as_"
      ; link_cmd = cmd ~prefix:"ld_"
      ; run_cmd = cmd ~prefix:"run_"
      }
  ;;

  (** Generate dune rules for the test. Requires project root path *)
  let gen test ~root : dune_rules =
    let input = Path.append Path.dot_dot test.path in
    let (Test (Targets tgt_names, run_spec)) = test.spec in

    let f acc tgt_name =
      let tgt = target_of_name tgt_name ~root in

      let compiled = Target.compile tgt ~input ~prefix:(Path.of_string "../tests") in
      let expected = compiled.rule :: acc.expected in

      let assembled =
        Target.assemble tgt { dir = Path.of_string "../expected"; art = compiled }
      in

      let runtime =
        Path.of_string (spf "back/%s/rukaml_stdlib.o" (Target.show_name tgt.name))
      in
      let runtime = Path.append Path.dot_dot @@ Path.append root runtime in

      let linked = Target.link tgt { dir = Path.dot; art = assembled } ~runtime in
      let artifacts = assembled.rule :: linked.rule :: acc.artifacts in

      let artifacts, cram =
        match run_spec with
        | Norun -> artifacts, acc.cram
        | Run (Exit exit, Stdout stdout) ->
          let art = Target.run tgt { dir = Path.dot_dot; art = linked } ~stdout ~exit in
          let cram_rule =
            spf
              {|(cram (applies_to %s) (deps ../%s))|}
              (String.chop_suffix_if_exists art.name ~suffix:".t")
              linked.name
          in
          art.rule :: artifacts, cram_rule :: acc.cram
      in

      { expected; artifacts; cram }
    in
    List.fold tgt_names ~init:rules_empty ~f
  ;;
end

let () =
  let root, test_paths =
    match
      Sys.get_argv () |> Array.to_list |> List.tl_exn |> List.map ~f:Path.of_string
    with
    | hd :: tl -> hd, tl
    | [] -> failwith "invalid argv"
  in

  let tests =
    List.map test_paths ~f:(fun path ->
      Test.{ path; spec = Result.ok_or_failwith (TestSpec.of_file path) })
  in
  let rules =
    List.fold tests ~init:Test.rules_empty ~f:(fun acc test ->
      let rules = Test.gen test ~root in
      { expected = List.append rules.expected acc.expected
      ; artifacts = List.append rules.artifacts acc.artifacts
      ; cram = List.append rules.cram acc.cram
      })
  in

  Out_channel.write_all "expected.dune" ~data:(String.concat_lines rules.expected);
  Out_channel.write_all "artifacts.dune" ~data:(String.concat_lines rules.artifacts);
  Out_channel.write_all "cram.dune" ~data:(String.concat_lines rules.cram)
;;
