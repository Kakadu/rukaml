(*
   test
  (targets rv32 amd64)
  (run (stdout
          "(1, one)"
          "(2, two)"
          "(3, three)"
          "(4, four)"
          "(5, five)"))
*)

let ls1 = [ 1; 2; 3; 4; 5 ]
let ls2 = [ "one"; "two"; "three"; "four"; "five" ]
let pp_pair x = printf "(%d, %s)\n" x

let rec pp_lists ls1 ls2 =
  match ls1, ls2 with
  | [], [] -> 0
  | x :: xs, y :: ys ->
    let t = pp_pair x y in
    pp_lists xs ys
  | _ -> 1
;;

let main = pp_lists ls1 ls2
