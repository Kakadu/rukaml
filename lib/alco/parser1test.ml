(*
   let others () =
     Alcotest.(check @@ float e) "0 is 0" 0. 0.;
     Alcotest.(check @@ float e) "0 is epsilon" 0. e;
     Alcotest.(check @@ neg @@ float e) "0 is not 1" 0. 1.;
     Alcotest.(check @@ neg @@ float e) "1 is not 0" 1. 0.;
     Alcotest.(check @@ float e) ".3 is .3" (0.1 +. 0.2) 0.3 *)

(* let others_set = [ ("others", `Quick, others) ] *)
module type PARSER = sig
  val parse_expr_string : string -> AST.expr option
end

let mkae_exprs (module Parser : PARSER) () =
  let open Alcotest in
  let make ?(desc = "") str ast =
    check
      (of_pp (Format.pp_print_option AST.pp_expr))
      desc
      (Parser.parse_expr_string str)
      (Some ast)
  in
  make "x" (EVar "x");
  make "x+1" (EBinop ("+", EVar "x", EConst 1));
  make "1+2*3" (EBinop ("+", EConst 1, EBinop ("*", EConst 2, EConst 3)));
  make "2*3+4" (EBinop ("+", EBinop ("*", EConst 2, EConst 3), EConst 4));
  make "x*3+y" (EBinop ("+", EBinop ("*", EVar "x", EConst 3), EVar "y"));
  ()

let () =
  Alcotest.run "Parser tests"
    [
      ("Manual", [ ("exprs", `Quick, mkae_exprs (module Parser)) ]);
      ("SIMD", [ ("exprs", `Quick, mkae_exprs (module ParseSIMD)) ]);
    ]
