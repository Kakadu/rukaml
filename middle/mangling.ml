open Frontend

let mangled_names : (Ident.t, Ident.t) Hashtbl.t = Hashtbl.create 100
let contains (ident : Ident.t) = Hashtbl.mem mangled_names ident

let find ({ hum_name; _ } as ident : Ident.t) =
  match hum_name with
  | "main" ->
    (* TODO : cludge *)
    Ident.ident "main" 0
  | _ -> Hashtbl.find mangled_names ident
;;

let clear_names () = Hashtbl.clear mangled_names

let pp_mangled_names ppf () =
  Format.fprintf ppf "{\n";
  Hashtbl.iter
    (fun k v -> Format.fprintf ppf "\t%a ~> %a;\n" Ident.pp k Ident.pp v)
    mangled_names;
  Format.fprintf ppf "}"
;;

let mangle_single_name (ident : Ident.t) =
  let buf = Buffer.create (String.length ident.hum_name) in
  String.iter
    (fun c ->
       match c with
       | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' -> Buffer.add_char buf c
       | _ -> Printf.bprintf buf "_code%u" (Char.code c))
    ident.hum_name;
  Buffer.add_string buf (Format.asprintf "_id%d" ident.id);
  Hashtbl.replace mangled_names ident (Ident.ident (Buffer.contents buf) ident.id)
;;

let run_single_test ident =
  clear_names ();
  mangle_single_name ident;
  pp_mangled_names Format.std_formatter ()
;;

let%expect_test _ =
  let ident = Ident.ident "let*>" 42 in
  run_single_test ident;
  [%expect
    {|
      {
      	let*> ~> let_code42_code62_id42;
      } |}]
;;

let%expect_test _ =
  let ident = Ident.ident "_'my'weird''name__" 666 in
  run_single_test ident;
  [%expect
    {|
      {
      	_'my'weird''name__ ~> __code39my_code39weird_code39_code39name___id666;
      } |}]
;;

let rec mangle_names_a ~(bounded : Ident.t list) : ANF.imm_expr -> ANF.imm_expr = function
  | (AUnit | AConst _ | APrimitive _) as i -> i
  | AVar v when contains v && not (List.mem v bounded) -> AVar (find v)
  | AVar _ as i -> i
  | ATuple (x1, x2, xs) ->
    ATuple
      ( mangle_names_a ~bounded x1
      , mangle_names_a ~bounded x2
      , List.map (mangle_names_a ~bounded) xs )
  | AConstruct (constr_name, fields) ->
    AConstruct (constr_name, List.map (mangle_names_a ~bounded) fields)
  | AArray items -> AArray (List.map (mangle_names_a ~bounded) items)
  | ALam (Apat_var v, rhs) -> ALam (Apat_var v, mangle_names_e ~bounded:(v :: bounded) rhs)
  | ALam (lhs, rhs) -> ALam (lhs, mangle_names_e ~bounded rhs)

and mangle_names_c ~(bounded : Ident.t list) : ANF.c_expr -> ANF.c_expr = function
  | CApp (x1, x2, xs) ->
    CApp
      ( (mangle_names_a ~bounded) x1
      , (mangle_names_a ~bounded) x2
      , List.map (mangle_names_a ~bounded) xs )
  | CIte (x1, x2, x3) ->
    CIte
      ( (mangle_names_c ~bounded) x1
      , (mangle_names_e ~bounded) x2
      , (mangle_names_e ~bounded) x3 )
  | CAtom atom -> CAtom (mangle_names_a ~bounded atom)

and mangle_names_e ~(bounded : Ident.t list) : ANF.expr -> ANF.expr = function
  | ELet (Frontend.Parsetree.NonRecursive, Apat_var v, rhs, body) ->
    ELet
      ( Frontend.Parsetree.NonRecursive
      , Apat_var v
      , mangle_names_c ~bounded rhs
      , mangle_names_e ~bounded:(v :: bounded) body )
  | ELet (Frontend.Parsetree.Recursive, Apat_var v, rhs, body) ->
    ELet
      ( Frontend.Parsetree.Recursive
      , Apat_var v
      , mangle_names_c ~bounded:(v :: bounded) rhs
      , mangle_names_e ~bounded:(v :: bounded) body )
  | ELet (flg, ((Apat_any | Apat_const _ | Apat_unit) as lhs), rhs, body) ->
    ELet (flg, lhs, mangle_names_c ~bounded rhs, mangle_names_e ~bounded body)
  | EComplex cexpr -> EComplex (mangle_names_c ~bounded cexpr)
;;

let mangle_names_stru_item ~(bounded : Ident.t list) : ANF.stru_item -> ANF.stru_item
  = function
  | ANF.ANF_vb (Frontend.Parsetree.NonRecursive, Apat_var name, rhs) ->
    let rhs = mangle_names_e ~bounded rhs in
    mangle_single_name name;
    let lhs = ANF.Apat_var (find name) in
    ANF.ANF_vb (Frontend.Parsetree.NonRecursive, lhs, rhs)
  | ANF.ANF_vb (Frontend.Parsetree.Recursive, Apat_var name, rhs) ->
    mangle_single_name name;
    let rhs = mangle_names_e ~bounded rhs in
    let lhs = ANF.Apat_var (find name) in
    ANF.ANF_vb (Frontend.Parsetree.Recursive, lhs, rhs)
  | ANF.ANF_vb (flg, ((Apat_any | Apat_const _ | Apat_unit) as lhs), rhs) ->
    ANF.ANF_vb (flg, lhs, mangle_names_e ~bounded rhs)
;;

let mangle_names_stru ~(bounded : Ident.t list) : ANF.stru -> ANF.stru =
  fun stru -> List.map (mangle_names_stru_item ~bounded) stru
;;

let bounded_for_tests = [ Ident.ident "print" 0; Ident.ident "exit" 0 ]

let run_single_test input =
  match Frontend.Parsing.parse_structure input with
  | Error err -> Frontend.Parsing.pp_error Format.std_formatter err
  | Ok ast ->
    (match Frontend.Inferencer.structure Frontend.Typedtree.empty_table ast with
     | Error err -> Frontend.Inferencer.pp_error Format.std_formatter err
     | Ok (_env, typedtree) ->
       clear_names ();
       let anf = ANF.anf_stru typedtree in
       Format.printf "anf stru:\n";
       ANF.pp_stru Format.std_formatter anf;
       Format.printf "\n\nmangled stru:\n";
       ANF.pp_stru Format.std_formatter (mangle_names_stru ~bounded:bounded_for_tests anf);
       Format.printf "\n\nmangled names:\n";
       pp_mangled_names Format.std_formatter ())
;;

let%expect_test "global constant shadowing" =
  let input =
    {|
        let x = 10
        let f () = x
        let x = 20
        let g () = x
      |}
  in
  run_single_test input;
  [%expect
    {|
      anf stru:
      let x =
                  10
                let f weird1 =
                  let () = weird1 in
                    x
                let x =
                  20
                let g weird2 =
                  let () = weird2 in
                    x

      mangled stru:
      let x_id53 =
                                       10
                                     let f_id54 weird1 =
                                       let () = weird1 in
                                         x_id53
                                     let x_id55 =
                                       20
                                     let g_id56 weird2 =
                                       let () = weird2 in
                                         x_id55

      mangled names:
      {
      	x ~> x_id55;
      	g ~> g_id56;
      	x ~> x_id53;
      	f ~> f_id54;
      } |}]
;;

let%expect_test "let rec" =
  let input =
    {|
      let rec fact n = if n < 1 then 1 else n * fact (n - 1)
      let main = 0
    |}
  in
  run_single_test input;
  [%expect
    {|
    anf stru:
    let rec fact n =
                let temp3 = (n < 1) in
                  let temp4 = (if temp3
                              then 1
                              else let temp5 = (n - 1) in
                                     let temp6 = fact temp5  in
                                       let temp7 = (n * temp6) in
                                         temp7) in
                    temp4
              let main =
                0

    mangled stru:
    let rec fact_id59 n =
                                   let temp3 = (n < 1) in
                                     let temp4 = (if temp3
                                                 then 1
                                                 else let temp5 = (n - 1) in
                                                        let temp6 = fact_id59 temp5  in
                                                          let temp7 = (n * temp6) in
                                                            temp7) in
                                       temp4
                                 let main =
                                   0

    mangled names:
    {
    	main ~> main_id61;
    	fact ~> fact_id59;
    } |}]
;;

let%expect_test "replacing built-in print with user-defined one" =
  let input =
    {|
      let () = print 1
      let print _ = ()
      let () = print 2
    |}
  in
  run_single_test input;
  [%expect
    {|
    anf stru:
    let () =
                let temp8 = print 1  in
                  temp8
              let print weird9 =
                let _ = weird9 in
                  0
              let () =
                let temp10 = print 2  in
                  temp10

    mangled stru:
    let () =
                                          let temp8 = print 1  in
                                            temp8
                                        let print_id67 weird9 =
                                          let _ = weird9 in
                                            0
                                        let () =
                                          let temp10 = print_id67 2  in
                                            temp10

    mangled names:
    {
    	print ~> print_id67;
    } |}]
;;

let%expect_test "shadowing global function with local one" =
  let input =
    {|
      let id x = x
      
      let () =
        let id y = y in
        id () 
      
      let () = id ()
    |}
  in
  run_single_test input;
  [%expect
    {|
    anf stru:
    let id x =
                x
              let () =
                let id y = y in
                  let temp11 = id 0  in
                    temp11
              let () =
                let temp12 = id 0  in
                  temp12

    mangled stru:
    let id_id71 x =
                                          x
                                        let () =
                                          let id y = y in
                                            let temp11 = id 0  in
                                              temp11
                                        let () =
                                          let temp12 = id_id71 0  in
                                            temp12

    mangled names:
    {
    	id ~> id_id71;
    } |}]
;;
