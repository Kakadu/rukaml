let patname =
  let open QCheck.Gen in
  pure (Printf.sprintf "%c%c%c")
  <*> char_range 'A' 'Z'
  <*> char_range 'a' 'z'
  <*> char_range 'a' 'z'
;;

(* Empty list or a singleton *)
let short_list arg =
  let open QCheck.Gen in
  oneof [ pure []; pure (fun x -> [ x ]) <*> arg ]
;;

type pattern =
  [%import:
    (Frontend.Parsetree.pattern
    [@with
      string := (string [@gen patname]);
      list := (list [@gen short_list (gen_pattern_sized (n / 3))])])]
[@@deriving qcheck]

let arbitrary_pattern_auto =
  let open QCheck.Iter in
  QCheck.make
    (gen_pattern_sized 5)
    ~print:(Format.asprintf "%a" Frontend.Pprint.pp_pattern)
;;

let varname =
  let open QCheck.Gen in
  pure (Printf.sprintf "%c%c%c")
  <*> char_range 'a' 'z'
  <*> char_range 'a' 'z'
  <*> char_range 'a' 'z'
;;

type const = [%import: Frontend.Parsetree.const] [@@deriving qcheck]
type rec_flag = [%import: Frontend.Parsetree.rec_flag] [@@deriving qcheck]

type expr =
  [%import:
    (Frontend.Parsetree.expr
    [@with
      string := (string [@gen varname]);
      list := (list [@gen short_list (gen_expr_sized (n / 3))])])]
[@@deriving qcheck]

let arbitrary_expr =
  let open QCheck.Iter in
  QCheck.make (gen_expr_sized 3) ~print:(Format.asprintf "%a" Frontend.Pprint.pp_expr)
;;

let run_pattern () =
  QCheck_runner.run_tests
    [ QCheck.(
        Test.make arbitrary_pattern_auto (fun l ->
          Result.ok l
          = Angstrom.parse_string
              ~consume:Angstrom.Consume.All
              Frontend.Parsing.pattern
              (Format.asprintf "%a" Frontend.Pprint.pp_pattern l)))
    ]
;;

let run_expr () =
  QCheck_runner.run_tests
    [ QCheck.(
        Test.make arbitrary_expr (fun l ->
          Result.ok l
          = Angstrom.parse_string
              ~consume:Angstrom.Consume.All
              Frontend.Parsing.(pack.expr_long pack)
              (Format.asprintf "%a" Frontend.Pprint.pp_expr l)))
    ]
;;
