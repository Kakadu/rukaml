open Miniml
open Parsetree

let reset_gensym, gensym =
  let n = ref 0 in
  ( (fun () -> n := 0)
  , fun () ->
      incr n;
      !n )
;;

let gensym_s : _ =
  fun ?(prefix = "temp") () ->
  let n = gensym () in
  Printf.sprintf "%s%d" prefix n
;;

let expr kname = function
  | x -> x
;;

let program : Parsetree.value_binding -> Parsetree.value_binding = function
  | Parsetree.NonRecursive, pat, body ->
    let kname = gensym_s ~prefix:"k" () in
    NonRecursive, pat, ELam (PVar kname, expr kname body)
  | Recursive, pat, body -> Recursive, pat, EVar "not_implemented"
;;
