open Miniml
open Parsetree

let __ () =
  let open Parsing in
  match parse "1" with
  | Result.Ok e -> Format.printf "%a\n%!" pp_expr e
  | Error s -> Format.eprintf "Parsing error: %a\n%!" pp_error s
;;

(* let __ () =
  let open Parsing in
  match Angstrom.parse_string ~consume:All Parsing.(typ) " int -> int" with
  | Result.Ok e -> Format.printf "%a\n%!" pp_typ e
  | Error s -> Format.eprintf "Parsing error: %s\n%!" s
;; *)

let pp_error ppf = function
  | #Parsing.error as e -> Parsing.pp_error ppf e
  | #Inferencer.error as e -> Inferencer.pp_error ppf e
;;

let wrap_e s =
  let ( let* ) = Base.Result.( >>= ) in
  let* ast = Parsing.parse s in
  print_endline @@ Parsetree.show_expr ast;
  let* typ = Inferencer.w ast in
  Base.Result.return (Format.printf "type is %a\n%!" Typedtree.pp_hum typ)
;;

(* | Result.Ok t ->
  | Error e -> Format.printf "Error: %a\n%!" Inferencer.pp_error e *)

let __ _ = wrap_e "1+2"
let __ _ = wrap_e "let i = fun x -> x in i"
let __ _ = wrap_e "let a = fun f -> fun x -> f x in a"
let __ _ = wrap_e "(fun x -> x)2"
let _ = wrap_e "let rec f = fun n -> f in f"
