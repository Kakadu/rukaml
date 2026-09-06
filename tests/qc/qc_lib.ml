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

let my_short_string =
  let open QCheck.Gen in
  string_small_of (char_range 'a' 'z')
  >>= fun s ->
  (* Printf.printf "%s %d\n" __FILE__ __LINE__; *)
  return s
;;

type const = Frontend.Parsetree.const =
  | PConst_int of (int[@gen QCheck.Gen.int_bound 1000])
  | PConst_char of char [@gen char_printable]
  | PConst_bool of bool
  | PConst_string of (string[@gen my_short_string])
[@@deriving qcheck]

type pattern = Frontend.Parsetree.pattern =
  | PUnit
  | PConst of const
  | PAny
  | PVar of (string[@gen patname])
  | PTuple of pattern * pattern * pattern list
  [@gen short_list (gen_pattern_sized (n / 3))]
  | PConstruct of string * pattern list [@gen short_list (gen_pattern_sized (n / 3))]
[@@deriving qcheck]

let ident_pat =
  let open QCheck.Gen in
  pure (fun a b -> PVar (Printf.sprintf "%c%c" a b))
  <*> char_range 'a' 'z'
  <*> char_range 'a' 'z'
;;

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

type rec_flag = Frontend.Parsetree.rec_flag =
  | Recursive
  | NonRecursive
[@@deriving qcheck]

type 'a list1 = 'a * 'a list [@@deriving qcheck]

type expr = Frontend.Parsetree.expr =
  | EUnit
  | EArray of expr list
  | EConst of const
  | EVar of (string[@gen varname])
  | EIf of expr * expr * expr
  | ELam of (pattern[@gen ident_pat]) * expr
  | EApp of expr * expr
  | ETuple of expr * expr * (expr list[@gen short_list (gen_expr_sized (n / 3))])
  | ELet of rec_flag * pattern * expr * expr
  | EConstruct of (string[@gen patname]) * expr list
  | EMatch of expr * (pattern * expr) list1
    (* [@with list := (list [@gen short_list (gen_expr_sized (n / 3))])] *)
[@@deriving qcheck]

let rec shrink_expr =
  let open QCheck.Iter in
  function
  | ELam (_, v) ->
    Printf.printf "%s %d\n" __FILE__ __LINE__;
    shrink_expr v
  | EApp (l, r) -> of_list [ l; r ]
  | EIf (a, b, c) -> of_list [ a; b; c ]
  | EConst (PConst_string s) when String.length s > 5 ->
    return (EConst (PConst_string (StringLabels.sub s ~pos:0 ~len:5)))
  | EConstruct (_, xs) -> of_list xs
  | _ ->
    print_endline "\nCalled\n";
    of_list []
;;

let arbitrary_expr =
  let open QCheck.Iter in
  QCheck.make
    (gen_expr_sized 3)
    ~shrink:shrink_expr
    ~print:(Format.asprintf "%a" Frontend.Pprint.pp_expr)
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
        Test.make ~name:"expr" arbitrary_expr (fun expected ->
          match
            Angstrom.parse_string
              ~consume:Angstrom.Consume.All
              Frontend.Parsing.(pack.expr_long pack)
              (Format.asprintf "%a" Frontend.Pprint.pp_expr expected)
          with
          | Result.Ok e when e = expected -> true
          | Ok e ->
            Format.printf "%a\n%!" Frontend.Parsetree.pp_expr e;
            false
          | Error _ ->
            Printf.eprintf "Can't parse\n";
            Format.eprintf "Initial AST: @[%a@]\n" Frontend.Parsetree.pp_expr expected;
            false))
    ]
;;
