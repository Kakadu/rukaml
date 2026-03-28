# Stdlib.List pattern matching are conveted into if-then-else trees

  $ run () { ../../driver/driver.exe $1 --target anf -o a.ml && cat a.ml; }

  $ run << EOF
  > let is_empty ls =
  >   match ls with
  >   | [] -> 0
  >   | _ -> 1
  > EOF
  let is_empty ls =
    let temp1 = ls in
      let temp2 = block_tag temp1  in
        (if (temp2 = 0)
        then 0
        else 1)

  $ run << EOF
  > let rec len ls =
  >   match ls with
  >   | [] -> 0
  >   | _ :: xs -> 1 + len xs
  > EOF
  let rec len ls =
    let temp1 = ls in
      let temp8 = block_tag temp1  in
        (if (temp8 = 0)
        then 0
        else let temp6 = block_tag temp1  in
               (if (temp6 = 1)
               then let temp4 = block_nth temp1 0 in
                      let xs = block_nth temp1 1 in
                        let temp2 = len xs  in
                          (1 + temp2)
               else match_failure))


  $ run << EOF
  > let rec map f ls =
  >   match ls with
  >   | [] -> []
  >   | hd :: tl -> f hd :: map f tl
  > EOF
  let rec map f ls =
    let temp1 = ls in
      let temp11 = block_tag temp1  in
        (if (temp11 = 0)
        then Constr_0
        else let temp9 = block_tag temp1  in
               (if (temp9 = 1)
               then let hd = block_nth temp1 0 in
                      let tl = block_nth temp1 1 in
                        let temp3 = f hd  in
                          let temp5 = map f tl in
                            (Constr_1 (temp3, temp5))
               else match_failure))

  $ run << EOF
  > let rec fold f acc ls =
  >   match ls with
  >   | [] -> acc
  >   | hd :: tl -> fold f (f acc hd) tl
  > EOF
  let rec fold f acc ls =
    let temp1 = ls in
      let temp11 = block_tag temp1  in
        (if (temp11 = 0)
        then acc
        else let temp9 = block_tag temp1  in
               (if (temp9 = 1)
               then let hd = block_nth temp1 0 in
                      let tl = block_nth temp1 1 in
                        let temp3 = f acc  in
                          let temp4 = temp3 hd  in
                            fold f temp4 tl
               else match_failure))


  $ run << EOF
  > let rec equal item_eq a b =
  >   match (a, b) with
  >   | [], [] -> true
  >   | x :: xs, y :: ys ->
  >     if item_eq x y then equal item_eq xs ys else false
  >   | _ -> false
  > EOF
  let rec equal item_eq a b =
    let temp2 = (a, b) in
      let temp19 = block_nth temp2 0 in
        let temp23 = block_tag temp19  in
          (if (temp23 = 0)
          then let temp20 = block_nth temp2 1 in
                 let temp21 = block_tag temp20  in
                   (if (temp21 = 0)
                   then 1
                   else let temp9 = block_nth temp2 0 in
                          let temp17 = block_tag temp9  in
                            (if (temp17 = 1)
                            then let x = block_nth temp9 0 in
                                   let xs = block_nth temp9 1 in
                                     let temp10 = block_nth temp2 1 in
                                       let temp13 = block_tag temp10  in
                                         (if (temp13 = 1)
                                         then let y = block_nth temp10 0 in
                                                let ys = block_nth temp10 1 in
                                                  let temp3 = item_eq x  in
                                                    let temp4 = temp3 y  in
                                                      (if temp4
                                                      then equal item_eq xs ys
                                                      else 0)
                                         else 0)
                            else 0))
          else let temp9 = block_nth temp2 0 in
                 let temp17 = block_tag temp9  in
                   (if (temp17 = 1)
                   then let x = block_nth temp9 0 in
                          let xs = block_nth temp9 1 in
                            let temp10 = block_nth temp2 1 in
                              let temp13 = block_tag temp10  in
                                (if (temp13 = 1)
                                then let y = block_nth temp10 0 in
                                       let ys = block_nth temp10 1 in
                                         let temp3 = item_eq x  in
                                           let temp4 = temp3 y  in
                                             (if temp4
                                             then equal item_eq xs ys
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
    let temp1 = ls in
      let temp10 = block_tag temp1  in
        (if (temp10 = 0)
        then 0
        else let temp8 = block_tag temp1  in
               (if (temp8 = 1)
               then let hd = block_nth temp1 0 in
                      let tl = block_nth temp1 1 in
                        let temp2 = pred hd  in
                          (if temp2
                          then 1
                          else exists pred tl)
               else match_failure))

  $ run << EOF
  > let rec forall pred ls =
  >   match ls with
  >   | [] -> true
  >   | hd :: tl -> if pred hd then forall pred tl else false
  > EOF
  let rec forall pred ls =
    let temp1 = ls in
      let temp10 = block_tag temp1  in
        (if (temp10 = 0)
        then 1
        else let temp8 = block_tag temp1  in
               (if (temp8 = 1)
               then let hd = block_nth temp1 0 in
                      let tl = block_nth temp1 1 in
                        let temp2 = pred hd  in
                          (if temp2
                          then forall pred tl
                          else 0)
               else match_failure))


  $ run << EOF
  > let rec join xs ys =
  >   match (xs, ys) with
  >   | [], _ -> []
  >   | _, [] -> []
  >   | xhd :: xtl, yhd :: ytl ->
  >     (xhd, yhd) :: join xtl ytl
  > EOF
  let rec join xs ys =
    let temp2 = (xs, ys) in
      let temp23 = block_nth temp2 0 in
        let temp25 = block_tag temp23  in
          (if (temp25 = 0)
          then let temp24 = block_nth temp2 1 in
                 Constr_0
          else let temp19 = block_nth temp2 0 in
                 let temp20 = block_nth temp2 1 in
                   let temp21 = block_tag temp20  in
                     (if (temp21 = 0)
                     then Constr_0
                     else let temp9 = block_nth temp2 0 in
                            let temp17 = block_tag temp9  in
                              (if (temp17 = 1)
                              then let xhd = block_nth temp9 0 in
                                     let xtl = block_nth temp9 1 in
                                       let temp10 = block_nth temp2 1 in
                                         let temp13 = block_tag temp10  in
                                           (if (temp13 = 1)
                                           then let yhd = block_nth temp10 0 in
                                                  let ytl = block_nth temp10 1 in
                                                    let temp5 = (xhd, yhd) in
                                                      let temp7 = join xtl ytl in
                                                        (Constr_1 (temp5, temp7))
                                           else match_failure)
                              else match_failure)))


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
    let temp1 = ls in
      let temp9 = block_tag temp1  in
        (if (temp9 = 0)
        then acc
        else let temp7 = block_tag temp1  in
               (if (temp7 = 1)
               then let hd = block_nth temp1 0 in
                      let tl = block_nth temp1 1 in
                        let temp3 = (Constr_1 (hd, acc)) in
                          __lifted_let_1_aux tl temp3
               else match_failure))
  let rev ls =
    let temp12 = Constr_0 in
      __lifted_let_1_aux ls temp12
  let rec __lifted_let_2_aux xs ys =
    let temp15 = (xs, ys) in
      let temp25 = block_nth temp15 0 in
        let temp27 = block_tag temp25  in
          (if (temp27 = 0)
          then block_nth temp15 1
          else let temp19 = block_nth temp15 0 in
                 let temp23 = block_tag temp19  in
                   (if (temp23 = 1)
                   then let hd = block_nth temp19 0 in
                          let tl = block_nth temp19 1 in
                            let acc = block_nth temp15 1 in
                              let temp17 = (Constr_1 (hd, acc)) in
                                __lifted_let_2_aux tl temp17
                   else match_failure))
  let cat xs ys =
    let temp29 = rev xs  in
      __lifted_let_2_aux temp29 ys

  $ run << EOF
  > let rec take n ls =
  >   match ls with
  >   | [] -> []
  >   | x :: xs ->
  >     if n = 0 then [] else x :: take (n - 1) xs
  > EOF
  let rec take n ls =
    let temp1 = ls in
      let temp14 = block_tag temp1  in
        (if (temp14 = 0)
        then Constr_0
        else let temp12 = block_tag temp1  in
               (if (temp12 = 1)
               then let x = block_nth temp1 0 in
                      let xs = block_nth temp1 1 in
                        (if (n = 0)
                        then Constr_0
                        else let temp5 = (n - 1) in
                               let temp7 = take temp5 xs in
                                 (Constr_1 (x, temp7)))
               else match_failure))


