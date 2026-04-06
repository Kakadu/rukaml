(*
   test
  (targets amd64)
  (run (stdout "123454321"))
*)

let pp_int oc n = fprintf oc "%d" n

type 'a my_list1 =
  | Nil1  
  | Cons1 of 'a * 'a my_list1 (* 2 fields *)
  
let rec pp_my_list1 oc pp_item ls =
  match ls with
  | Cons1 (hd, tl) ->
    let () = pp_item oc hd in
    pp_my_list1 oc pp_item tl
| Nil1 -> ()

let () = pp_my_list1 stdout pp_int (Cons1 (1, Cons1 (2, Cons1 (3, Cons1 (4, Cons1 (5, Nil1)))))) 

type 'a my_list2 =
  | Nil2  
  | Cons2 of ('a * 'a my_list2) (* 1 field which is tuple of 2 *)

let rec pp_my_list2 oc pp_item ls =
  match ls with
  | Cons2 (hd, tl) ->
    let () = pp_item oc hd in
    pp_my_list2 oc pp_item tl
| Nil2 -> ()


let () = pp_my_list2 stdout pp_int (Cons2 (4, Cons2 (3, Cons2 (2, Cons2 (1, Nil2))))) 

let main = 0
