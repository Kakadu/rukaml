open AST

include struct
  open QCheck.Gen

  let infix_op = oneof [ return "+" ]

  let ident_gen =
    (* add_shrink_invariant (fun s -> not (Parser.is_keyword s))
       @@ *)
    map2 (Printf.sprintf "%c%c") (char_range 'a' 'z')
      (oneof [ char_range 'a' 'z'; numeral ])
  (* >>= fun s -> if Parser.is_keyword s then oneof [] else return s *)

  let expr_gen =
    sized
    @@ fix (fun self -> function
         | 0 -> oneof [ return (EConst 42) ]
         | n ->
             let half = self (n / 10) in
             oneof [ map3 (fun op l r -> EBinop (op, l, r)) infix_op half half ])

  let stmt_gen =
    sized
    @@ fix (fun self -> function
         | n ->
             let half = self (n / 3) in
             oneof
               [
                 map2 (fun op e -> Assgn (op, e)) ident_gen expr_gen;
                 map3
                   (fun cond th el -> Ite (cond, [ th ], [ el ]))
                   expr_gen half half;
                 map2 (fun op e -> While (op, [ e ])) expr_gen half;
               ])
end

let make_print_parse ?name gen ~of_string ~to_string eq =
  QCheck.Test.make ?name gen (fun c1 ->
      match to_string c1 with
      | exception exc1 ->
          Printf.printf "ERROR: %s %s %d\n%!"
            (Stdlib.Printexc.to_string exc1)
            __FILE__ __LINE__;
          false
      | j -> (
          match of_string j with
          | c2 -> eq c1 c2
          | exception exc1 ->
              Printf.printf "ERROR while parsing string %S: %s %s %d\n%!" j
                (Stdlib.Printexc.to_string exc1)
                __FILE__ __LINE__;
              print_endline (Printexc.get_backtrace ());
              false))

include struct
  open QCheck.Iter
  (* open QCheck.Shrink *)

  let rec shrink_expr : expr QCheck.Shrink.t = function
    | EVar _ -> empty
    | EConst i -> QCheck.Shrink.int i >|= fun i -> EConst i
    | EBinop (_, l, r) -> shrink_expr l <+> shrink_expr r

  (*
  let rec shrink_stmts single = function
    | [] -> empty
    | h :: tl -> single h <+> shrink_stmts single tl *)
  let map3 f a b c yield =
    a (fun x -> b (fun y -> c (fun z -> yield (f x y z))))

  let rec shrink_stmt : stmt QCheck.Shrink.t = function
    | Assgn (lhs, rhs) ->
        (* shrink_expr rhs <+> *)
        shrink_expr rhs >|= fun rhs -> Assgn (lhs, rhs)
    | Ite (cond, th, el) ->
        map3
          (fun e th el -> Ite (e, th, el))
          (shrink_expr cond)
          (QCheck.Shrink.list ~shrink:shrink_stmt th)
          (QCheck.Shrink.list ~shrink:shrink_stmt el)
    | While (cond, body) ->
        (* shrink_expr cond
           <+> shrink_stmts shrink_stmt body
           <+> *)
        map2
          (fun cond st -> While (cond, st))
          (shrink_expr cond)
          (QCheck.Shrink.list ~shrink:shrink_stmt body)
end

include struct
  open QCheck.Iter

  let arbitrary_identifier =
    QCheck.make ident_gen ~print:(Format.asprintf "%a" Pprint.pp_ident)
  (* ~shrink:shrink_ident *)

  let arbitrary_expression =
    QCheck.make expr_gen
      ~print:(Format.asprintf "%a" Pprint.pp_expr)
      ~shrink:shrink_expr

  let arbitrary_stmt =
    QCheck.make stmt_gen
      ~print:(Format.asprintf "%a" Pprint.pp_stmt)
      ~shrink:shrink_stmt
end

let print_parse_is_identity_identifier =
  make_print_parse arbitrary_identifier
    ~of_string:(fun x -> Option.get (Parser.parse_ident_string x))
    ~to_string:Pprint.show_ident Stdlib.( = )

let print_parse_is_identity_expression =
  make_print_parse arbitrary_expression
    ~of_string:(fun x -> Option.get (Parser.parse_expr_string x))
    ~to_string:Pprint.show_expr Stdlib.( = )

let print_parse_is_identity_stmt =
  make_print_parse ~name:"print/parse stmt" arbitrary_stmt
    ~of_string:(fun x -> Option.get (Parser.parse_stmt_string x))
    ~to_string:Pprint.show_stmt Stdlib.( = )

type cfg = {
  mutable cfg_expr : bool;
  mutable cfg_stmt : bool; (* mutable cfg_fun_m : bool; *)
}

let cfg = { cfg_expr = false; cfg_stmt = false (* cfg_fun_m = true  *) }

let () =
  Arg.parse
    [
      ("-expr", Arg.Unit (fun () -> cfg.cfg_expr <- true), "");
      ("-stmt", Arg.Unit (fun () -> cfg.cfg_stmt <- true), "");
      (* ("-fun-m", Arg.Unit (fun () -> cfg.cfg_fun_m <- true), ""); *)
    ]
    (fun _ -> assert false)
    "";

  QCheck_base_runner.set_seed 227547128;
  Format.printf "Failed ident tests: %d\n%!"
  @@ QCheck_base_runner.run_tests [ print_parse_is_identity_identifier ];
  Format.printf "Failed expr tests: %d\n%!"
  @@ QCheck_base_runner.run_tests [ print_parse_is_identity_expression ];
  Format.printf "Failed stmt tests: %d\n%!"
  @@ QCheck_base_runner.run_tests [ print_parse_is_identity_stmt ];
  (* if cfg.cfg_rule then
       Format.printf "Failed rule tests: %d\n%!"
       @@ QCheck_base_runner.run_tests [ print_parse_is_identity_rule ];
     if cfg.cfg_cond then
       Format.printf "Failed condition tests: %d\n%!"
       @@ QCheck_base_runner.run_tests [ print_parse_is_identity_condition ]; *)
  ()
