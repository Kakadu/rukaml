  $ run () { ../../driver/driver.exe $1 --target anf -o a.ml && cat a.ml; }

constants matching
  $ run << EOF
  > let () = ()
  > EOF
  let _ = ()

  $ run << EOF
  > let "123" = "123"
  > EOF
  let "123" = "123"

  $ run << EOF
  > let 1, true = 0, false
  > EOF
  let tuple2 =
    (0, false)
  let 1 = block_nth tuple2 0
  let true = block_nth tuple2 1

  $ run << EOF
  > let (1, 2) = 1, 2
  > EOF
  let tuple2 =
    (1, 2)
  let 1 = block_nth tuple2 0
  let 2 = block_nth tuple2 1

---

  $ run << EOF
  > let swap (x, y) = y, x
  > EOF
  let swap tuple1 =
    let x = block_nth tuple1 0 in
      let y = block_nth tuple1 1 in
        (y, x)


  $ run << EOF
  > let (x, y) = 1, 2
  > EOF
  let tuple2 =
    (1, 2)
  let x =
    block_nth tuple2 0
  let y =
    block_nth tuple2 1


---
  $ run << EOF
  > let [ 3 ] = [ 1 + 2 ]
  > EOF
  let adt3 =
    let temp1 = (1 + 2) in
      (Constr_1 (temp1, Constr_0))
  let 1 = block_tag adt3 
  let 3 = block_nth adt3 0
  let adt4 =
    block_nth adt3 1
  let 0 = block_tag adt4 

  $ run << EOF
  > let is_empty [] = true
  > EOF
  let is_empty adt1 =
    let tag2 = block_tag adt1  in
      (if (tag2 = 0)
      then true
      else match_failure)

---

nested lets
  $ run << EOF
  > let swap (x, y) = y, x
  > EOF
  let swap tuple1 =
    let x = block_nth tuple1 0 in
      let y = block_nth tuple1 1 in
        (y, x)

 

TODO
  $ run << EOF  
  > let _ = 1, 2
  > EOF
  let _ = let temp1 = (1, 2) in
            temp1

  $ run << EOF  
  > let main =
  >   let (x, y) = (1, 2) in
  > x
  > EOF
  let main =
    let tuple2 = (1, 2) in
      let x = block_nth tuple2 0 in
        let y = block_nth tuple2 1 in
          x

  $ run << EOF  
  > let [ x ] = [ 42 ]
  > EOF
  let adt2 =
    (Constr_1 (42, Constr_0))
  let 1 = block_tag adt2 
  let x =
    block_nth adt2 0
  let adt3 =
    block_nth adt2 1
  let 0 = block_tag adt3 


  $ run << EOF  
  > let main =
  >   let [ x ] = [ 42 ] in
  > x
  > EOF
  let main =
    let adt2 = (Constr_1 (42, Constr_0)) in
      let tag5 = block_tag adt2  in
        (if (tag5 = 1)
        then let x = block_nth adt2 0 in
               let field3 = block_nth adt2 1 in
                 let tag4 = block_tag field3  in
                   (if (tag4 = 0)
                   then x
                   else match_failure)
        else match_failure)



  $ run << EOF  
  > let main =
  >   let [ (1, 2); (3, 4) ] = [ (5, 6) ] in
  > 0
  > EOF
  let main =
    let temp1 = (5, 6) in
      let adt3 = (Constr_1 (temp1, Constr_0)) in
        let tag14 = block_tag adt3  in
          (if (tag14 = 1)
          then let field4 = block_nth adt3 0 in
                 let 1 = block_nth field4 0 in
                   let 2 = block_nth field4 1 in
                     let field7 = block_nth adt3 1 in
                       let tag13 = block_tag field7  in
                         (if (tag13 = 1)
                         then let field8 = block_nth field7 0 in
                                let 3 = block_nth field8 0 in
                                  let 4 = block_nth field8 1 in
                                    let field11 = block_nth field7 1 in
                                      let tag12 = block_tag field11  in
                                        (if (tag12 = 0)
                                        then 0
                                        else match_failure)
                         else match_failure)
          else match_failure)


  $ run << EOF  
  > let main =
  >   let _ = 1 + 2 in
  > 0
  > EOF
  let main =
    let _ = (1 + 2) in
      0


  $ run << EOF  
  > let () = printf "123"
  > EOF
  let _ = let temp1 = printf "123"  in
            temp1

  $ run << EOF  
  > let main =
  >   let () = printf "123" in
  > 0
  > EOF
  let main =
    let () = printf "123"  in
      0

  $ run << EOF  
  > let _ = printf "123"
  > EOF
  let _ = let temp1 = printf "123"  in
            temp1
