(*
test
  (targets (anf promote) (amd64 promote))
  (run (stdout "rukaml_print_int 0"))
*)

(* should print 42 then 0 *)

let large x = if x then print 0 else print 1
let main =
  let x = if (if (if false
                  then false else (let t42 = print 42 in true))
              then false else true)
          then false else true in
  large x
