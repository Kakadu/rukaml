  $ run () { ../../driver/driver.exe $1 --target cconv -o a.ml && cat a.ml; }

# without renaming
$ run << EOF
> let foo () = 1
> 
> let () =
>   let foo () = 2 in
>   ()
> 
> EOF
let foo () = 1
let foo () = 2
let () = ()

# with renaming
  $ run << EOF
  > let foo () = 1
  > 
  > let () =
  >   let foo () = 2 in
  >   ()
  > 
  > EOF
  let foo () = 1
  let __lifted_let_1_foo () = 2
  let () = ()
