# TMatch expressions are converted into if-then-else tree in ANF.ml

  $ run () { ../../driver/driver.exe $1 --target anf -o a.ml && cat a.ml; }

  $ run << EOF
  > let main =
  >   match (1, 2) with
  >   | (x, y) -> x + y
  > EOF
  let main =
    let temp2 = (1, 2) in
      let x = get_arg 0 temp2 in
        let y = get_arg 1 temp2 in
          (x + y)

  $ run << EOF
  > let main =
  >   match (1, true) with
  >   | (1, true) -> true
  >   | (n, true) -> n = 1
  > EOF
  let main =
    let temp2 = (1, true) in
      let temp7 = get_arg 0 temp2 in
        (if (temp7 = 1)
        then let temp8 = get_arg 1 temp2 in
               (if (temp8 = true)
               then 1
               else let n = get_arg 0 temp2 in
                      let temp5 = get_arg 1 temp2 in
                        (if (temp5 = true)
                        then (n = 1)
                        else match_failure))
        else let n = get_arg 0 temp2 in
               let temp5 = get_arg 1 temp2 in
                 (if (temp5 = true)
                 then (n = 1)
                 else match_failure))

# exhausive
  $ run << EOF
  > let main =
  >   match (1, true) with
  >   | (1, true) -> true
  >   | x -> false
  > EOF
  let main =
    let temp2 = (1, true) in
      let temp3 = get_arg 0 temp2 in
        (if (temp3 = 1)
        then let temp4 = get_arg 1 temp2 in
               (if (temp4 = true)
               then 1
               else let x = temp2 in
                      0)
        else let x = temp2 in
               0)

  $ run << EOF
  > let main =
  >   match (1, true) with
  >   | (1, true) -> true
  >   | _ -> false
  > EOF
  let main =
    let temp2 = (1, true) in
      let temp3 = get_arg 0 temp2 in
        (if (temp3 = 1)
        then let temp4 = get_arg 1 temp2 in
               (if (temp4 = true)
               then 1
               else 0)
        else 0)

  $ run << EOF
  > let main =
  >   match (1, true) with
  >   | (1, true) -> true
  >   | (x, y) -> false
  > EOF
  let main =
    let temp2 = (1, true) in
      let temp5 = get_arg 0 temp2 in
        (if (temp5 = 1)
        then let temp6 = get_arg 1 temp2 in
               (if (temp6 = true)
               then 1
               else let x = get_arg 0 temp2 in
                      let y = get_arg 1 temp2 in
                        0)
        else let x = get_arg 0 temp2 in
               let y = get_arg 1 temp2 in
                 0)
#

# non exhausive
  $ run << EOF
  > let main =
  >   match (1, true) with
  >   | (1, true) -> true
  >   | (x, false) -> false
  > EOF
  let main =
    let temp2 = (1, true) in
      let temp6 = get_arg 0 temp2 in
        (if (temp6 = 1)
        then let temp7 = get_arg 1 temp2 in
               (if (temp7 = true)
               then 1
               else let x = get_arg 0 temp2 in
                      let temp4 = get_arg 1 temp2 in
                        (if (temp4 = false)
                        then 0
                        else match_failure))
        else let x = get_arg 0 temp2 in
               let temp4 = get_arg 1 temp2 in
                 (if (temp4 = false)
                 then 0
                 else match_failure))

  $ run << EOF
  > let main =
  >   match (1, true) with
  >   | (1, true) -> true
  >   | (0, x) -> false
  > EOF
  let main =
    let temp2 = (1, true) in
      let temp6 = get_arg 0 temp2 in
        (if (temp6 = 1)
        then let temp7 = get_arg 1 temp2 in
               (if (temp7 = true)
               then 1
               else let temp3 = get_arg 0 temp2 in
                      (if (temp3 = 0)
                      then let x = get_arg 1 temp2 in
                             0
                      else match_failure))
        else let temp3 = get_arg 0 temp2 in
               (if (temp3 = 0)
               then let x = get_arg 1 temp2 in
                      0
               else match_failure))

  $ run << EOF
  > let main =
  >   match (1, true) with
  >   | (1, true) -> true
  >   | (0, false) -> false
  > EOF
  let main =
    let temp2 = (1, true) in
      let temp7 = get_arg 0 temp2 in
        (if (temp7 = 1)
        then let temp8 = get_arg 1 temp2 in
               (if (temp8 = true)
               then 1
               else let temp3 = get_arg 0 temp2 in
                      (if (temp3 = 0)
                      then let temp4 = get_arg 1 temp2 in
                             (if (temp4 = false)
                             then 0
                             else match_failure)
                      else match_failure))
        else let temp3 = get_arg 0 temp2 in
               (if (temp3 = 0)
               then let temp4 = get_arg 1 temp2 in
                      (if (temp4 = false)
                      then 0
                      else match_failure)
               else match_failure))
#

# assert that some cases are unreachable
  $ run << EOF
  > let main =
  >   match 1 with
  >   | 1 -> 1
  >   | 2 -> 2
  >   | _ -> 3
  >   | _ -> 4
  > EOF
  let main =
    let temp1 = 1 in
      (if (temp1 = 1)
      then 1
      else (if (temp1 = 2)
           then 2
           else 3))

  $ run << EOF
  > let main =
  >   match (1, 2) with
  >   | (1, 2) -> 1
  >   | (x, y) -> 2
  >   | (x, _) -> 3
  >   | (_, y) -> 4
  >   | (_, _) -> 5
  >   | _ -> 6
  > EOF
  let main =
    let temp2 = (1, 2) in
      let temp11 = get_arg 0 temp2 in
        (if (temp11 = 1)
        then let temp12 = get_arg 1 temp2 in
               (if (temp12 = 2)
               then 1
               else let x = get_arg 0 temp2 in
                      let y = get_arg 1 temp2 in
                        2)
        else let x = get_arg 0 temp2 in
               let y = get_arg 1 temp2 in
                 2)
#

  $ run << EOF
  > let main =
  >   match (1, true) with
  >   | (1, true) -> 0
  >   | (n, true) -> 1
  >   | (1, b) -> 2
  >   | (n, b) -> 3
  > EOF
  let main =
    let temp2 = (1, true) in
      let temp11 = get_arg 0 temp2 in
        (if (temp11 = 1)
        then let temp12 = get_arg 1 temp2 in
               (if (temp12 = true)
               then 0
               else let n = get_arg 0 temp2 in
                      let temp9 = get_arg 1 temp2 in
                        (if (temp9 = true)
                        then 1
                        else let temp5 = get_arg 0 temp2 in
                               (if (temp5 = 1)
                               then let b = get_arg 1 temp2 in
                                      2
                               else let n = get_arg 0 temp2 in
                                      let b = get_arg 1 temp2 in
                                        3)))
        else let n = get_arg 0 temp2 in
               let temp9 = get_arg 1 temp2 in
                 (if (temp9 = true)
                 then 1
                 else let temp5 = get_arg 0 temp2 in
                        (if (temp5 = 1)
                        then let b = get_arg 1 temp2 in
                               2
                        else let n = get_arg 0 temp2 in
                               let b = get_arg 1 temp2 in
                                 3)))

  $ run << EOF
  > let main =
  >   match (1, true) with
  >   | (n, true) -> 0
  >   | (1, b) -> 1
  > EOF
  let main =
    let temp2 = (1, true) in
      let n = get_arg 0 temp2 in
        let temp7 = get_arg 1 temp2 in
          (if (temp7 = true)
          then 0
          else let temp3 = get_arg 0 temp2 in
                 (if (temp3 = 1)
                 then let b = get_arg 1 temp2 in
                        1
                 else match_failure))

  $ run << EOF
  > let main =
  >   match (1, true) with
  >   | (1, true) -> 0
  >   | (n, true) -> 1
  >   | (1, b) -> 2
  > EOF
  let main =
    let temp2 = (1, true) in
      let temp9 = get_arg 0 temp2 in
        (if (temp9 = 1)
        then let temp10 = get_arg 1 temp2 in
               (if (temp10 = true)
               then 0
               else let n = get_arg 0 temp2 in
                      let temp7 = get_arg 1 temp2 in
                        (if (temp7 = true)
                        then 1
                        else let temp3 = get_arg 0 temp2 in
                               (if (temp3 = 1)
                               then let b = get_arg 1 temp2 in
                                      2
                               else match_failure)))
        else let n = get_arg 0 temp2 in
               let temp7 = get_arg 1 temp2 in
                 (if (temp7 = true)
                 then 1
                 else let temp3 = get_arg 0 temp2 in
                        (if (temp3 = 1)
                        then let b = get_arg 1 temp2 in
                               2
                        else match_failure)))

  $ run << EOF
  > let main =
  >   match (1, true) with
  >   | (1, true) -> 0
  >   | (n, true) -> 1
  >   | (1, b) -> 2
  >   | (n, b) -> 3
  > EOF
  let main =
    let temp2 = (1, true) in
      let temp11 = get_arg 0 temp2 in
        (if (temp11 = 1)
        then let temp12 = get_arg 1 temp2 in
               (if (temp12 = true)
               then 0
               else let n = get_arg 0 temp2 in
                      let temp9 = get_arg 1 temp2 in
                        (if (temp9 = true)
                        then 1
                        else let temp5 = get_arg 0 temp2 in
                               (if (temp5 = 1)
                               then let b = get_arg 1 temp2 in
                                      2
                               else let n = get_arg 0 temp2 in
                                      let b = get_arg 1 temp2 in
                                        3)))
        else let n = get_arg 0 temp2 in
               let temp9 = get_arg 1 temp2 in
                 (if (temp9 = true)
                 then 1
                 else let temp5 = get_arg 0 temp2 in
                        (if (temp5 = 1)
                        then let b = get_arg 1 temp2 in
                               2
                        else let n = get_arg 0 temp2 in
                               let b = get_arg 1 temp2 in
                                 3)))

  $ run << EOF
  > let main =
  >   match (1, true) with
  >   | (1, true) -> true
  >   | (n1, true) -> n1 = 1
  >   | (n2, b) -> if b then n2 = 1 else false
  > EOF
  let main =
    let temp2 = (1, true) in
      let temp11 = get_arg 0 temp2 in
        (if (temp11 = 1)
        then let temp12 = get_arg 1 temp2 in
               (if (temp12 = true)
               then 1
               else let n1 = get_arg 0 temp2 in
                      let temp9 = get_arg 1 temp2 in
                        (if (temp9 = true)
                        then (n1 = 1)
                        else let n2 = get_arg 0 temp2 in
                               let b = get_arg 1 temp2 in
                                 (if b
                                 then (n2 = 1)
                                 else 0)))
        else let n1 = get_arg 0 temp2 in
               let temp9 = get_arg 1 temp2 in
                 (if (temp9 = true)
                 then (n1 = 1)
                 else let n2 = get_arg 0 temp2 in
                        let b = get_arg 1 temp2 in
                          (if b
                          then (n2 = 1)
                          else 0)))


  $ run << EOF
  > let main =
  >   match (1 :: [ 2 ]) with
  >   | [] -> 0
  >   | _ :: _ -> 1
  > EOF
  let main =
    let temp1 = Constr_0 in
      let temp2 = (Constr_1 (2, temp1)) in
        let temp4 = (Constr_1 (1, temp2)) in
          let temp9 = get_tag temp4  in
            (if (temp9 = 0)
            then 0
            else let temp7 = get_tag temp4  in
                   (if (temp7 = 1)
                   then let temp5 = get_arg 0 temp4 in
                          let temp6 = get_arg 1 temp4 in
                            1
                   else match_failure))

  $ run << EOF
  > let main =
  >   match (true, false) with
  >   | (true, false) -> 0
  >   | (true, true) -> 1
  >   | (false, true) -> 2
  > EOF
  let main =
    let temp2 = (true, false) in
      let temp11 = get_arg 0 temp2 in
        (if (temp11 = true)
        then let temp12 = get_arg 1 temp2 in
               (if (temp12 = false)
               then 0
               else let temp7 = get_arg 0 temp2 in
                      (if (temp7 = true)
                      then let temp8 = get_arg 1 temp2 in
                             (if (temp8 = true)
                             then 1
                             else let temp3 = get_arg 0 temp2 in
                                    (if (temp3 = false)
                                    then let temp4 = get_arg 1 temp2 in
                                           (if (temp4 = true)
                                           then 2
                                           else match_failure)
                                    else match_failure))
                      else let temp3 = get_arg 0 temp2 in
                             (if (temp3 = false)
                             then let temp4 = get_arg 1 temp2 in
                                    (if (temp4 = true)
                                    then 2
                                    else match_failure)
                             else match_failure)))
        else let temp7 = get_arg 0 temp2 in
               (if (temp7 = true)
               then let temp8 = get_arg 1 temp2 in
                      (if (temp8 = true)
                      then 1
                      else let temp3 = get_arg 0 temp2 in
                             (if (temp3 = false)
                             then let temp4 = get_arg 1 temp2 in
                                    (if (temp4 = true)
                                    then 2
                                    else match_failure)
                             else match_failure))
               else let temp3 = get_arg 0 temp2 in
                      (if (temp3 = false)
                      then let temp4 = get_arg 1 temp2 in
                             (if (temp4 = true)
                             then 2
                             else match_failure)
                      else match_failure)))

  $ run << EOF
  > let main =
  >   match (true, false) with
  >   | (x, false) -> 1
  >   | (true, x) -> 2
  >   | x -> 3
  > EOF
  let main =
    let temp2 = (true, false) in
      let x = get_arg 0 temp2 in
        let temp7 = get_arg 1 temp2 in
          (if (temp7 = false)
          then 1
          else let temp3 = get_arg 0 temp2 in
                 (if (temp3 = true)
                 then let x = get_arg 1 temp2 in
                        2
                 else let x = temp2 in
                        3))


  $ run << EOF
  > let main =
  >   match (true, false) with
  >   | (x, false) -> 1
  >   | (x, x) -> 2
  >   | _ -> 3
  > EOF
  let main =
    let temp2 = (true, false) in
      let x = get_arg 0 temp2 in
        let temp6 = get_arg 1 temp2 in
          (if (temp6 = false)
          then 1
          else let x = get_arg 0 temp2 in
                 let x = get_arg 1 temp2 in
                   2)


  $ run << EOF
  > let main =
  >   match (true, false) with
  >   | (x, false) -> 1
  >   | (false, x) -> 2
  >   | (true, x) -> 3
  >   | _ -> 4
  > EOF
  let main =
    let temp2 = (true, false) in
      let x = get_arg 0 temp2 in
        let temp10 = get_arg 1 temp2 in
          (if (temp10 = false)
          then 1
          else let temp6 = get_arg 0 temp2 in
                 (if (temp6 = false)
                 then let x = get_arg 1 temp2 in
                        2
                 else let temp3 = get_arg 0 temp2 in
                        (if (temp3 = true)
                        then let x = get_arg 1 temp2 in
                               3
                        else 4)))

  $ run << EOF
  > let main =
  >   match [ true; false ] with
  >   | [] -> 0
  >   | _ -> 1
  > EOF
  let main =
    let temp1 = Constr_0 in
      let temp2 = (Constr_1 (false, temp1)) in
        let temp4 = (Constr_1 (true, temp2)) in
          let temp5 = get_tag temp4  in
            (if (temp5 = 0)
            then 0
            else 1)


  $ run << EOF
  > let main =
  >   match [ true; false ] with
  >   | [] -> 0
  >   | [ x ] -> 1
  >   | _ -> 2
  > EOF
  let main =
    let temp1 = Constr_0 in
      let temp2 = (Constr_1 (false, temp1)) in
        let temp4 = (Constr_1 (true, temp2)) in
          let temp11 = get_tag temp4  in
            (if (temp11 = 0)
            then 0
            else let temp9 = get_tag temp4  in
                   (if (temp9 = 1)
                   then let x = get_arg 0 temp4 in
                          let temp6 = get_arg 1 temp4 in
                            let temp7 = get_tag temp6  in
                              (if (temp7 = 0)
                              then 1
                              else 2)
                   else 2))



  $ run << EOF
  > let is_empty =
  >   match [ 1 ] with
  >   | [] -> true
  >   | _ -> false
  > EOF
  let is_empty =
    let temp1 = Constr_0 in
      let temp3 = (Constr_1 (1, temp1)) in
        let temp4 = get_tag temp3  in
          (if (temp4 = 0)
          then 1
          else 0)

  $ run << EOF
  > let main =
  >   match 1 with
  >   | 1 -> 1
  >   | 2 -> 2
  > EOF
  let main =
    let temp1 = 1 in
      (if (temp1 = 1)
      then 1
      else (if (temp1 = 2)
           then 2
           else match_failure))


  $ run << EOF
  > let main =
  >   match 1 with
  >   | 1 -> 1
  >   | 2 -> 2
  >   | _ -> 3
  > EOF
  let main =
    let temp1 = 1 in
      (if (temp1 = 1)
      then 1
      else (if (temp1 = 2)
           then 2
           else 3))


  $ run << EOF
  > let main =
  >   match (3, 4) with
  >   | 1, 2 -> 5
  >   | 1, y -> 6
  > EOF
  let main =
    let temp2 = (3, 4) in
      let temp6 = get_arg 0 temp2 in
        (if (temp6 = 1)
        then let temp7 = get_arg 1 temp2 in
               (if (temp7 = 2)
               then 5
               else let temp3 = get_arg 0 temp2 in
                      (if (temp3 = 1)
                      then let y = get_arg 1 temp2 in
                             6
                      else match_failure))
        else let temp3 = get_arg 0 temp2 in
               (if (temp3 = 1)
               then let y = get_arg 1 temp2 in
                      6
               else match_failure))
