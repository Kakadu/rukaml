# Stdlib.List pattern matching are conveted into if-then-else trees

  $ run () { ../../driver/driver.exe $1 --target anf -o a.ml && cat a.ml; }

  $ run << EOF
  > let is_empty ls =
  >   match ls with
  >   | [] -> 0
  >   | _ -> 1
  > EOF
  let is_empty ls =
    (if (ls = 0)
    then 0
    else 1)

  $ run << EOF
  > let rec len ls =
  >   match ls with
  >   | [] -> 0
  >   | _ :: xs -> 1 + len xs
  > EOF
  let rec len ls =
    (if (ls = 0)
    then 0
    else let temp7 = block_tag ls  in
         (if (temp7 = 1)
         then let temp5 = block_nth ls 0 in
              let temp6 = block_nth ls 1 in
              let temp2 = len temp6  in
              (1 + temp2)
         else match_failure 666 ))


  $ run << EOF
  > let rec map f ls =
  >   match ls with
  >   | [] -> []
  >   | hd :: tl -> f hd :: map f tl
  > EOF
  let rec map f ls =
    (if (ls = 0)
    then Constr_0
    else let hd = block_nth ls 0 in
         let tl = block_nth ls 1 in
         let temp2 = f hd  in
         let temp4 = map f tl in
         (Constr_1 (temp2, temp4)))

  $ run << EOF
  > let rec fold f acc ls =
  >   match ls with
  >   | [] -> acc
  >   | hd :: tl -> fold f (f acc hd) tl
  > EOF
  let rec fold f acc ls =
    (if (ls = 0)
    then acc
    else let hd = block_nth ls 0 in
         let tl = block_nth ls 1 in
         let temp3 = f acc  in
         let temp4 = temp3 hd  in
         fold f temp4 tl)


  $ run << EOF
  > let rec equal item_eq a b =
  >   match (a, b) with
  >   | [], [] -> true
  >   | x :: xs, y :: ys ->
  >     if item_eq x y then equal item_eq xs ys else false
  >   | _ -> false
  > EOF
  let rec equal item_eq a b =
    let temp1 = (a, b) in
      let temp20 = block_nth temp1 0 in
      (if (temp20 = 0)
      then let temp21 = block_nth temp1 1 in
           (if (temp21 = 0)
           then 1
           else let temp10 = block_nth temp1 0 in
                let temp18 = block_tag temp10  in
                (if (temp18 = 1)
                then let temp16 = block_nth temp10 0 in
                     let temp17 = block_nth temp10 1 in
                     let temp11 = block_nth temp1 1 in
                     let temp14 = block_tag temp11  in
                     (if (temp14 = 1)
                     then let temp12 = block_nth temp11 0 in
                          let temp13 = block_nth temp11 1 in
                          let temp3 = item_eq temp16  in
                          let temp4 = temp3 temp12  in
                          (if temp4
                          then equal item_eq temp17 temp13
                          else 0)
                     else 0)
                else 0))
      else let temp10 = block_nth temp1 0 in
           let temp18 = block_tag temp10  in
           (if (temp18 = 1)
           then let temp16 = block_nth temp10 0 in
                let temp17 = block_nth temp10 1 in
                let temp11 = block_nth temp1 1 in
                let temp14 = block_tag temp11  in
                (if (temp14 = 1)
                then let temp12 = block_nth temp11 0 in
                     let temp13 = block_nth temp11 1 in
                     let temp3 = item_eq temp16  in
                     let temp4 = temp3 temp12  in
                     (if temp4
                     then equal item_eq temp17 temp13
                     else 0)
                else 0)
           else 0))


  $ run << EOF
  > let rec exists pred ls =
  >   match ls with
  >   | [] -> false
  >   | hd :: tl -> if pred hd then true else exists pred tl
  > EOF
  let rec exists pred ls =
    (if (ls = 0)
    then 0
    else let hd = block_nth ls 0 in
         let tl = block_nth ls 1 in
         let temp2 = pred hd  in
         (if temp2
         then 1
         else exists pred tl))

  $ run << EOF
  > let rec forall pred ls =
  >   match ls with
  >   | [] -> true
  >   | hd :: tl -> if pred hd then forall pred tl else false
  > EOF
  let rec forall pred ls =
    (if (ls = 0)
    then 1
    else let hd = block_nth ls 0 in
         let tl = block_nth ls 1 in
         let temp2 = pred hd  in
         (if temp2
         then forall pred tl
         else 0))


  $ run << EOF
  > let rec join xs ys =
  >   match (xs, ys) with
  >   | [], _ -> []
  >   | _, [] -> []
  >   | xhd :: xtl, yhd :: ytl ->
  >     (xhd, yhd) :: join xtl ytl
  > EOF
  let rec join xs ys =
    let temp1 = (xs, ys) in
      let temp23 = block_nth temp1 0 in
      (if (temp23 = 0)
      then let temp24 = block_nth temp1 1 in
           Constr_0
      else let temp20 = block_nth temp1 0 in
           let temp21 = block_nth temp1 1 in
           (if (temp21 = 0)
           then Constr_0
           else let temp10 = block_nth temp1 0 in
                let temp18 = block_tag temp10  in
                (if (temp18 = 1)
                then let temp16 = block_nth temp10 0 in
                     let temp17 = block_nth temp10 1 in
                     let temp11 = block_nth temp1 1 in
                     let temp14 = block_tag temp11  in
                     (if (temp14 = 1)
                     then let temp12 = block_nth temp11 0 in
                          let temp13 = block_nth temp11 1 in
                          let temp5 = (temp16, temp12) in
                            let temp7 = join temp17 temp13 in
                            (Constr_1 (temp5, temp7))
                     else match_failure 666 )
                else match_failure 666 )))


  $ run << EOF
  > let rev ls =
  >   let rec aux ls acc =
  >     match ls with
  >     | [] -> acc
  >     | hd :: tl -> aux tl (hd :: acc)
  >   in
  >   aux ls []
  > let cat xs ys =
  >   let rec aux xs ys =
  >     match (xs, ys) with
  >     | [], acc -> acc
  >     | hd :: tl, acc -> aux tl (hd :: acc)
  >    in
  >  aux (rev xs) ys  
  > EOF
  let rec __lifted_let_1_aux ls acc =
    (if (ls = 0)
    then acc
    else let hd = block_nth ls 0 in
         let tl = block_nth ls 1 in
         let temp3 = (Constr_1 (hd, acc)) in
         __lifted_let_1_aux tl temp3)
  let rev ls =
    let temp6 = Constr_0 in
    __lifted_let_1_aux ls temp6
  let rec __lifted_let_2_aux xs ys =
    let temp8 = (xs, ys) in
      let temp20 = block_nth temp8 0 in
      (if (temp20 = 0)
      then let temp21 = block_nth temp8 1 in
           temp21
      else let temp14 = block_nth temp8 0 in
           let temp18 = block_tag temp14  in
           (if (temp18 = 1)
           then let temp16 = block_nth temp14 0 in
                let temp17 = block_nth temp14 1 in
                let temp15 = block_nth temp8 1 in
                let temp11 = (Constr_1 (temp16, temp15)) in
                __lifted_let_2_aux temp17 temp11
           else match_failure 666 ))
  let cat xs ys =
    let temp23 = rev xs  in
    __lifted_let_2_aux temp23 ys

  $ run << EOF
  > let rec take n ls =
  >   match ls with
  >   | [] -> []
  >   | x :: xs ->
  >     if n = 0 then [] else x :: take (n - 1) xs
  > EOF
  let rec take n ls =
    (if (ls = 0)
    then Constr_0
    else let x = block_nth ls 0 in
         let xs = block_nth ls 1 in
         (if (n = 0)
         then Constr_0
         else let temp4 = (n - 1) in
              let temp6 = take temp4 xs in
              (Constr_1 (x, temp6))))


