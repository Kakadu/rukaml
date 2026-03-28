i think it is required to rename the lifted lambda or let‑in during lifting in cconv, otherwise it may shadow the global declaration
(TODO? it could be improved by maintaining table of globals and keeping the original name if it shadows nothing (but then the generated code would be less consistent) )

  $ run () { ../../driver/driver.exe $1 --target cconv -o a.ml && cat a.ml; }

before fix
(global qwe should not be shadowed by the local one)
$ run << EOF
> let qwe () = 0
> let asd () =
>   let qwe () = 1 in
> 2
> 
> let main = qwe ()
> EOF
let qwe () = 0
let qwe () = 1
let asd () = 2
let main = qwe ()

after fix
  $ run << EOF
  > let qwe () = 0
  > let asd () =
  >   let qwe () = 1 in
  > 2
  > 
  > let main = qwe ()
  > EOF
  let qwe () = 0
  let __lifted_let_1_qwe () = 1
  let asd () = 2
  let main = qwe ()

before fix
(local aux should not shadow the global one)
$ run << EOF
> let string expected (str, pos) =
>   let rec aux i =
>     if i >= string_len expected
>     then return expected (str, pos + i)
>     else if pos + i >= string_len str
>     then Prez_error Perr_unexpected_eof
>     else if string_nth (pos + i) str = string_nth i expected
>     then aux (i + 1)
>     else Prez_error (Perr_message "unexpected string")
>   in
>   aux 0
> ;;
> EOF
let rec aux string_nth string_len str return pos expected i =
if i >= string_len expected
then return expected (
str, pos + i) 
else if (pos + i) >= string_len str
then Prez_error Perr_unexpected_eof
else 
if string_nth (pos + i) str = string_nth i expected
then aux string_nth string_len str return pos expected (i + 1)
else Prez_error (Perr_message "unexpected string")
let string expected (str, pos) = aux string_nth string_len str return pos expected 0

after fix
  $ run << EOF
  > let string expected (str, pos) =
  >   let rec aux i =
  >     if i >= string_len expected
  >     then return expected (str, pos + i)
  >     else if pos + i >= string_len str
  >     then Prez_error Perr_unexpected_eof
  >     else if string_nth (pos + i) str = string_nth i expected
  >     then aux (i + 1)
  >     else Prez_error (Perr_message "unexpected string")
  >   in
  >   aux 0
  > ;;
  > EOF
  let rec __lifted_let_1_aux str return pos expected i = if i >= string_len expected
                                                         then return expected (
                                                         str, pos + i) 
                                                         else if (pos + i) >= string_len str
                                                              then Prez_error Perr_unexpected_eof
                                                              else if string_nth (pos + i) str = string_nth i expected
                                                                   then __lifted_let_1_aux str return pos expected (i + 1)
                                                                   else Prez_error (Perr_message "unexpected string")
  let string expected (str, pos) = __lifted_let_1_aux str return pos expected 0


---

assert that names of "nested"s are different
  $ run << EOF
  > let f x =
  >   let nested x = x in
  > 1
  > let g x =
  >   let nested x = x in
  > 2
  > EOF
  let __lifted_let_1_nested x = x
  let f x = 1
  let __lifted_let_2_nested x = x
  let g x = 2

assert that "nested" is renamed both in toplevel declaration and foo's body 
  $ run << EOF
  > let foo x =
  >  let nested y = y in
  > nested x
  > EOF
  let __lifted_let_1_nested y = y
  let foo x = __lifted_let_1_nested x

assert that fact is renamed everywhere
  $ run << EOF
  > let main =
  >   let rec fact n = if n < 1 then 1 else n * fact (n - 1) in
  > fact 5
  > EOF
  let rec __lifted_let_1_fact n = if n < 1 then 1 else n * __lifted_let_1_fact (n - 1)
  let main = __lifted_let_1_fact 5

---

nested lambdas

  $ run << EOF
  > let main =
  >   let ls = [ fun id -> id ] in
  > 0
  > EOF
  let __lifted_lam_1 id = id
  let main = let ls = [ __lifted_lam_1 ] in 0

  $ run << EOF
  > let main =
  >   let x = 0 in
  > fun x -> x
  > EOF
  let __lifted_lam_1 x = x
  let main = let x = 0 in __lifted_lam_1

  $ run << EOF
  > let main =
  >   let f x = 0 in
  > fun x -> f x
  > EOF
  let __lifted_lam_1 f x = f x
  let __lifted_let_2_f x = 0
  let main = __lifted_lam_1 __lifted_let_2_f


  $ run << EOF
  > let call f () () = f ()
  > let call f () = call ()
  > EOF
  let call f () () = f ()
  let call f () = call ()
