open! Base
open Stdio

module Path = File_path.Relative
module PPart = File_path.Part

let spf = Printf.sprintf

(** Compilation target *)
module Target = struct
  type t =
    { name : string
    ; assemble_cmd : string option
    ; link_cmd : string option
    ; run_cmd : string option
    }

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
 (mode %s)
 (action
  (run %%{project_root}/driver/driver.exe %%{deps} --target %s %s -o %%{target})))
|}
  ;;

  (** Compile input file to target's IL *)
  let compile (tgt : t) ~name ~(input : Path.t) ~flags ~promote : [ `Compile ] art =
    let name = String.concat [ name; "."; tgt.name; ".out" ] in
    let rule =
      spf
        compile_rule
        name
        (Path.to_string input)
        (if promote then "promote" else "standard")
        tgt.name
        (String.concat ~sep:" " flags)
    in
    { name; rule }
  ;;

  let assemble_rule =
    Stdlib.format_of_string
      {|
(rule
  (target %s)
  (deps %s)
  (mode (promote (until-clean) (into obj)))
  (action (system "%s %%{deps} -o %%{target}")))
|}
  ;;

  open Option
  let ( let* ) = ( >>= )

  (** Assemble target's IL to arch-specific object file *)
  let assemble tgt (input : [ `Compile ] art_located) : [ `Assemble ] art option =
    let* cmd = tgt.assemble_cmd in
    let name = String.chop_suffix_if_exists input.art.name ~suffix:".out" ^ ".o" in
    let input = Path.append_part input.dir (PPart.of_string input.art.name) in
    let rule = spf assemble_rule name (Path.to_string input) cmd in
    return { name; rule }
  ;;

  let link_rule =
    Stdlib.format_of_string
      {|
(rule
  (target %s)
  (deps %s %s)
  (mode (promote (until-clean) (into exe)))
  (action (system "%s %%{deps} -o %%{target}")))
|}
  ;;

  (** Link object file with runtime to produce executable *)
  let link tgt (input : [ `Assemble ] art_located) ~(runtime : Path.t)
    : [ `Link ] art option
    =
    let* cmd = tgt.link_cmd in
    let name = String.chop_suffix_if_exists input.art.name ~suffix:".o" ^ ".exe" in
    let input = Path.append_part input.dir (PPart.of_string input.art.name) in
    let rule = spf link_rule name (Path.to_string runtime) (Path.to_string input) cmd in
    return { name; rule }
  ;;

  let run_rule =
    Stdlib.format_of_string
      {|
(rule
  (mode (promote (until-clean) (into cram)))
  (action (with-stdout-to %s
    (echo "  $ %s %s%s\n%s"))))
|}
  ;;

  (** Run (or rather generate cram test to run) executable
      with expected stdout and exit code *)
  let run tgt (input : [ `Link ] art_located) ~(stdout : string list) ~(exit : int)
    : [ `Run ] art option
    =
    let* cmd = tgt.run_cmd in
    let name = String.chop_suffix_if_exists input.art.name ~suffix:".exe" ^ ".t" in
    let input = Path.append_part input.dir (PPart.of_string input.art.name) in
    let stdout = List.map stdout ~f:(String.append "\n  ") |> String.concat in
    let exit = if exit = 0 then "" else spf "  [%d]\n" exit in
    let rule = spf run_rule name cmd (Path.to_string input) stdout exit in
    return { name; rule }
  ;;
end

(** Some combinators to parse S-expressions *)
module SexpParser = struct
  type 'a result =
    | Failed of string
    | Parsed of 'a * Sexp.t list

  type 'a t = Sexp.t list -> 'a result

  let fail msg _ = Failed msg
  let return x inp = Parsed (x, inp)

  let ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t =
    fun p f inp ->
    match p inp with
    | Parsed (r, rest) -> f r rest
    | Failed _ as err -> err
  ;;
  let ( let* ) = ( >>= )

  let ( >>| ) p f = p >>= fun x -> return (f x)

  let ( *> ) p1 p2 = p1 >>= fun _ -> p2

  let ( <|> ) p1 p2 inp =
    match p1 inp with
    | Failed _ -> p2 inp
    | Parsed _ as ok -> ok
  ;;

  let choice : 'a t list -> 'a t = function
    | hd :: tl -> List.fold_left ~f:( <|> ) ~init:hd tl
    | [] -> fail "empty choice"
  ;;

  let option default p inp =
    match p inp with
    | Failed _ -> return default inp
    | Parsed _ as ok -> ok
  ;;

  let rec many p inp =
    match p inp with
    | Failed _ -> return [] inp
    | Parsed (r, rest) -> (many p >>= fun rest -> return (r :: rest)) rest
  ;;

  let many1 p =
    let* r = p in
    let* rest = many p in
    return (r :: rest)
  ;;

  (** Parse single sexp atom *)
  let atom : string t = function
    | Sexp.Atom x :: tl -> return x tl
    | _ :: _ -> Failed "not an atom"
    | [] -> Failed "end of input"
  ;;

  (** Parse expected string in sexp atom *)
  let string expected : unit t =
    atom
    >>= function
    | actual when String.equal expected actual -> return ()
    | actual -> fail (spf "wrong string: expected '%s', actual '%s'" expected actual)
  ;;

  (** Parse sexp list with provided parser *)
  let list (p : 'a t) : 'a t = function
    | Sexp.List hd :: tl ->
      (match p hd with
       | Parsed (r, []) -> Parsed (r, tl)
       | Parsed (_, _) -> Failed "list not consumed"
       | Failed _ as err -> err)
    | _ :: _ -> Failed "not a list"
    | [] -> Failed "end of input"
  ;;
end

(** Test specification parsed from user comments *)
module TestSpec = struct
  type target =
    { name : string
    ; promote : bool
    }

  type run =
    { exit : int
    ; stdout : string list
    }

  type t =
    { src : Path.t option
    ; targets : target list
    ; flags : string list list
    ; run : run option
    }

  open SexpParser

  (** name | (name promote) *)
  let ptarget =
    let pdefault =
      let* name = atom in
      return { name; promote = false }
    in
    let ppromoted =
      let* name = atom in
      let* () = string "promote" in
      return { name; promote = true }
    in
    choice [ pdefault; list ppromoted ]
  ;;

  (** run (exit ..) (stdout ..) *)
  let prun =
    let pexit =
      string "exit" *> atom
      >>| Int.of_string_opt
      >>= function
      | Some code -> return code
      | None -> fail "invalid code"
    in
    let pstdout = string "stdout" *> many atom in

    let* () = string "run" in
    let* exit = option 0 (list pexit) in
    let* stdout = option [] (list pstdout) in
    return { exit; stdout }
  ;;

  let pspec =
    let* () = string "test" in
    let psrc = string "src" *> atom >>| Path.of_string in
    let pflags = string "flags" *> many1 (list (many atom)) in

    let* src = option None (list psrc >>| Option.some) in
    let* targets = list (string "targets" *> many1 ptarget) in
    let* flags = option [ [] ] (list pflags) in
    let* run = option None (list prun >>| Option.some) in
    return { src; targets; flags; run }
  ;;

  (** Parse test spec from the file *)
  let of_file (path : Path.t) =
    let ( let* ), return = Result.( >>= ), Result.return in
    let fail msg = Result.fail @@ msg ^ spf " in %s" (Path.to_string path) in

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

    (* goes through the file line by line until
       it fully parses the first comment *)
    let parse = Angstrom.Buffered.feed @@ Angstrom.Buffered.parse pcomment in
    let* comment = In_channel.with_file (Path.to_string path) ~f:(parse_next parse) in

    let* sexp =
      match Parsexp.Many.parse_string comment with
      | Error err -> fail (Parsexp.Parse_error.message err)
      | Ok [] -> fail "test spec not found"
      | Ok list -> return list
    in
    match pspec sexp with
    | Parsed (r, []) -> return r
    | Parsed _ -> fail "invalid spec"
    | Failed msg -> fail msg
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

  let build_target name ~root =
    let read_cmd ~prefix =
      let path = Path.append_part root @@ PPart.of_string (prefix ^ name) in
      let text =
        try
          In_channel.with_file
            (Path.to_string path)
            ~f:(Fn.compose Option.some In_channel.input_all)
        with
        | Sys_error _ -> None
      in
      match text with
      | Some "none" -> None
      | _ -> text
    in
    Target.
      { name
      ; assemble_cmd = read_cmd ~prefix:"as_"
      ; link_cmd = read_cmd ~prefix:"ld_"
      ; run_cmd = read_cmd ~prefix:"run_"
      }
  ;;

  (** Generate dune rules for the test. Requires project root path *)
  let gen test ~root : dune_rules =
    (* the name for build artifact is derrived from
       the location of the input file and its name *)
    let name =
      Path.chop_prefix_if_exists test.path ~prefix:(Path.of_string "tests")
      |> Path.to_parts
      |> List.map ~f:PPart.to_string
      |> String.concat ~sep:"."
      |> String.chop_suffix_if_exists ~suffix:".ml"
    in

    let input = Path.append Path.dot_dot test.path in
    let input =
      Option.value_map
        test.spec.src
        ~default:input
        ~f:(Path.append (Path.dirname_defaulting_to_dot input))
    in

    let ( >>= ) = Option.( >>= ) in

    let gen_tgt (spec : TestSpec.target) ~suffix { expected; artifacts; cram } flags =
      let tgt = build_target spec.name ~root in

      let compiled =
        Target.compile tgt ~name:(name ^ suffix) ~input ~flags ~promote:spec.promote
      in
      let expected = compiled.rule :: expected in

      let assembled =
        Target.assemble tgt { dir = Path.of_string "../expected"; art = compiled }
      in
      let artifacts =
        Option.value_map assembled ~default:artifacts ~f:(fun art ->
          art.rule :: artifacts)
      in

      let runtime = Path.of_string (spf "back/%s/rukaml_stdlib.o" tgt.name) in
      let runtime = Path.append Path.dot_dot @@ Path.append root runtime in
      (* link only if assemble succeeded *)
      let linked =
        assembled >>= fun art -> Target.link tgt { dir = Path.dot; art } ~runtime
      in
      let artifacts =
        Option.value_map linked ~default:artifacts ~f:(fun art -> art.rule :: artifacts)
      in

      let gen_run (spec : TestSpec.run) =
        linked
        >>= fun linked ->
        Target.run
          tgt
          { dir = Path.dot_dot; art = linked }
          ~exit:spec.exit
          ~stdout:spec.stdout
        >>= fun art ->
        let cram_rule =
          spf
            "(cram (applies_to %s) (deps ../%s))"
            (String.chop_suffix_if_exists art.name ~suffix:".t")
            linked.name
        in
        Some (art.rule, cram_rule)
      in

      let artifacts, cram =
        let default = artifacts, cram in
        match test.spec.run with
        | None -> default
        | Some spec ->
          Option.value_map (gen_run spec) ~default ~f:(fun (rule, cram_rule) ->
            rule :: artifacts, cram_rule :: cram)
      in

      { expected; artifacts; cram }
    in

    List.fold test.spec.targets ~init:rules_empty ~f:(fun acc tgt ->
      List.foldi test.spec.flags ~init:acc ~f:(fun idx ->
        gen_tgt tgt ~suffix:(".fl" ^ Int.to_string idx)))
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
