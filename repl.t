  $ cat << EOF | ./REPL.exe -
  > 1+2
  > EOF
  parsing a string '1+2'
  Parsed: EApp (EApp (EVar ("+"), EConst (1)), EConst (2))
  Result: int
  $ cat << EOF | ./REPL.exe -
  > fun x -> 1
  > EOF
  parsing a string 'fun x -> 1'
  Parsed: ELam (PVar ("x"), EConst (1))
  Result: ('_0 -> int)
  $ cat << EOF | ./REPL.exe -
  > fun f -> fun x -> f x
  > EOF
  parsing a string 'fun f -> fun x -> f x'
  Parsed: ELam (PVar ("f"), ELam (PVar ("x"), EApp (EVar ("f"), EVar ("x"))))
  Result: (('_1 -> '_2) -> ('_1 -> '_2))
  $ cat << EOF | ./REPL.exe -
  > fun f -> fun x -> f
  > EOF
  parsing a string 'fun f -> fun x -> f'
  Parsed: ELam (PVar ("f"), ELam (PVar ("x"), EVar ("f")))
  Result: ('_0 -> ('_1 -> '_0))
  $ cat << EOF | ./REPL.exe -
  > let f = fun x -> x in f
  > EOF
  parsing a string 'let f = fun x -> x in f'
  Parsed: ELet
          (NonRecursive, PVar ("f"), ELam (PVar ("x"), EVar ("x")), EVar ("f"))
  Result: ('_1 -> '_1)
  $ cat << EOF | ./REPL.exe -
  > fun x -> 1+1
  > EOF
  parsing a string 'fun x -> 1+1'
  Parsed: ELam (PVar ("x"), EApp (EApp (EVar ("+"), EConst (1)), EConst (1)))
  Result: ('_0 -> int)
  $ cat << EOF | ./REPL.exe -
  > fun x -> if x then 1 else x
  > EOF
  parsing a string 'fun x -> if x then 1 else x'
  Parsed: ELam (PVar ("x"), EIf (EVar ("x"), EConst (1), EVar ("x")))
  Error: unification failed on int and bool
# Y combinator
  $ cat << EOF | ./REPL.exe -
  > fun f -> (fun x -> f (x x)) (fun x -> f (x x))
  > EOF
  parsing a string 'fun f -> (fun x -> f (x x)) (fun x -> f (x x))'
  Parsed: ELam
          (PVar ("f"),
           EApp
           (ELam (PVar ("x"), EApp (EVar ("f"), EApp (EVar ("x"), EVar ("x")))),
            ELam (PVar ("x"), EApp (EVar ("f"), EApp (EVar ("x"), EVar ("x"))))))
  Error: Occurs check failed
  $ cat << EOF | ./REPL.exe -
  > (fun fix -> fun f -> f (fix f))
  > EOF
  parsing a string '(fun fix -> fun f -> f (fix f))'
  Parsed: ELam
          (PVar ("fix"),
           ELam
           (PVar ("f"), EApp (EVar ("f"), EApp (EVar ("fix"), EVar ("f")))))
  Result: ((('_2 -> '_3) -> '_2) -> (('_2 -> '_3) -> '_3))
  $ cat << EOF | ./REPL.exe -
  > let rec s f g x = f x (g x) in s
  > EOF
  parsing a string 'let rec s f g x = f x (g x) in s'
  Parsed: ELet
          (Recursive, PVar ("s"),
           ELam
           (PVar ("f"),
            ELam
            (PVar ("g"),
             ELam
             (PVar ("x"),
              EApp
              (EApp (EVar ("f"), EVar ("x")), EApp (EVar ("g"), EVar ("x")))))),
           EVar ("s"))
  Result: (('_3 -> ('_5 -> '_6)) -> (('_3 -> '_5) -> ('_3 -> '_6)))
# Z combinator
  $ cat << EOF | ./REPL.exe -
  > fun f -> (fun x -> f (fun v -> x x v)) (fun x -> f (fun v -> x x v))
  > EOF
  parsing a string 'fun f -> (fun x -> f (fun v -> x x v)) (fun x -> f (fun v -> x x v))'
  Parsed: ELam
          (PVar ("f"),
           EApp
           (ELam
            (PVar ("x"),
             EApp
             (EVar ("f"),
              ELam
              (PVar ("v"), EApp (EApp (EVar ("x"), EVar ("x")), EVar ("v"))))),
            ELam
            (PVar ("x"),
             EApp
             (EVar ("f"),
              ELam
              (PVar ("v"), EApp (EApp (EVar ("x"), EVar ("x")), EVar ("v")))))))
  Error: Occurs check failed
  $ cat << EOF | ./REPL.exe -
  > let rec f = fun n -> f in f
  > EOF
  parsing a string 'let rec f = fun n -> f in f'
  Parsed: ELet
          (Recursive, PVar ("f"), ELam (PVar ("n"), EVar ("f")), EVar ("f"))
  Error: Occurs check failed
  $ cat << EOF | ./REPL.exe -
  > let rec fac = fun n -> n*fac in fac
  > EOF
  parsing a string 'let rec fac = fun n -> n*fac in fac'
  Parsed: ELet
          (Recursive, PVar ("fac"),
           ELam
           (PVar ("n"), EApp (EApp (EVar ("*"), EVar ("n")), EVar ("fac"))),
           EVar ("fac"))
  Error: unification failed on int and (int -> int)
  $ cat << EOF | ./REPL.exe -
  > let rec zed f x = f (zed f) x in
  > let fac = fun self -> fun n -> if n=1 then 1 else n * (self (n-1)) in
  > zed fac
  > EOF
  parsing a string 'let rec zed f x = f (zed f) x in
  let fac = fun self -> fun n -> if n=1 then 1 else n * (self (n-1)) in
  zed fac'
  Parsed: ELet
          (Recursive, PVar ("zed"),
           ELam
           (PVar ("f"),
            ELam
            (PVar ("x"),
             EApp
             (EApp (EVar ("f"), EApp (EVar ("zed"), EVar ("f"))), EVar ("x")))),
           ELet
           (NonRecursive, PVar ("fac"),
            ELam
            (PVar ("self"),
             ELam
             (PVar ("n"),
              EIf
              (EApp (EApp (EVar ("="), EVar ("n")), EConst (1)), EConst (1),
               EApp
               (EApp (EVar ("*"), EVar ("n")),
                EApp
                (EVar ("self"),
                 EApp (EApp (EVar ("-"), EVar ("n")), EConst (1))))))),
            EApp (EVar ("zed"), EVar ("fac"))))
  Result: (int -> int)
  $ cat << EOF | ./REPL.exe -
  > let id = fun x -> x in
  > let idd = fun x -> x in
  > (id idd) (id 1)
  > EOF
  parsing a string 'let id = fun x -> x in
  let idd = fun x -> x in
  (id idd) (id 1)'
  Parsed: ELet
          (NonRecursive, PVar ("id"), ELam (PVar ("x"), EVar ("x")),
           ELet
           (NonRecursive, PVar ("idd"), ELam (PVar ("x"), EVar ("x")),
            EApp
            (EApp (EVar ("id"), EVar ("idd")), EApp (EVar ("id"), EConst (1)))))
  Result: int
# zed combiantor. Expecting - : (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b = <fun>
  $ cat << EOF | ./REPL.exe -
  > let rec fix f = f (fix f) in fix
  > EOF
  parsing a string 'let rec fix f = f (fix f) in fix'
  Parsed: ELet
          (Recursive, PVar ("fix"),
           ELam
           (PVar ("f"), EApp (EVar ("f"), EApp (EVar ("fix"), EVar ("f")))),
           EVar ("fix"))
  Result: (('_3 -> '_3) -> '_3)
  $ cat << EOF | ./REPL.exe -
  > let rec fix f = f (fix f)  in
  > let fac = fun self -> fun n -> if n=1 then 1 else n * (self (n-1)) in
  > fix fac
  > EOF
  parsing a string 'let rec fix f = f (fix f)  in
  let fac = fun self -> fun n -> if n=1 then 1 else n * (self (n-1)) in
  fix fac'
  Parsed: ELet
          (Recursive, PVar ("fix"),
           ELam
           (PVar ("f"), EApp (EVar ("f"), EApp (EVar ("fix"), EVar ("f")))),
           ELet
           (NonRecursive, PVar ("fac"),
            ELam
            (PVar ("self"),
             ELam
             (PVar ("n"),
              EIf
              (EApp (EApp (EVar ("="), EVar ("n")), EConst (1)), EConst (1),
               EApp
               (EApp (EVar ("*"), EVar ("n")),
                EApp
                (EVar ("self"),
                 EApp (EApp (EVar ("-"), EVar ("n")), EConst (1))))))),
            EApp (EVar ("fix"), EVar ("fac"))))
  Result: (int -> int)
  $ cat << EOF | ./REPL.exe -
  > let rec zed f x = f (zed f) x in
  > let fac = fun self -> fun n -> if n=1 then 1 else n * (self (n-1)) in
  > zed fac
  > EOF
  parsing a string 'let rec zed f x = f (zed f) x in
  let fac = fun self -> fun n -> if n=1 then 1 else n * (self (n-1)) in
  zed fac'
  Parsed: ELet
          (Recursive, PVar ("zed"),
           ELam
           (PVar ("f"),
            ELam
            (PVar ("x"),
             EApp
             (EApp (EVar ("f"), EApp (EVar ("zed"), EVar ("f"))), EVar ("x")))),
           ELet
           (NonRecursive, PVar ("fac"),
            ELam
            (PVar ("self"),
             ELam
             (PVar ("n"),
              EIf
              (EApp (EApp (EVar ("="), EVar ("n")), EConst (1)), EConst (1),
               EApp
               (EApp (EVar ("*"), EVar ("n")),
                EApp
                (EVar ("self"),
                 EApp (EApp (EVar ("-"), EVar ("n")), EConst (1))))))),
            EApp (EVar ("zed"), EVar ("fac"))))
  Result: (int -> int)
$ cat << EOF | ./REPL.exe -
> (fun fix -> fun f -> f (fix f))
> EOF
$ cat << EOF | ./REPL.exe -
> let rec s f g x = f x (g x) in s
> EOF
  $ cat << EOF | ./REPL.exe -
  > let rec fac = fun n -> if n=1 then 1 else n * (fac (n-1)) in fac
  > EOF
  parsing a string 'let rec fac = fun n -> if n=1 then 1 else n * (fac (n-1)) in fac'
  Parsed: ELet
          (Recursive, PVar ("fac"),
           ELam
           (PVar ("n"),
            EIf
            (EApp (EApp (EVar ("="), EVar ("n")), EConst (1)), EConst (1),
             EApp
             (EApp (EVar ("*"), EVar ("n")),
              EApp
              (EVar ("fac"), EApp (EApp (EVar ("-"), EVar ("n")), EConst (1)))))),
           EVar ("fac"))
  Result: (int -> int)
  $ cat << EOF | ./REPL.exe -
  > fun f -> fun x -> f (f x)
  > EOF
  parsing a string 'fun f -> fun x -> f (f x)'
  Parsed: ELam
          (PVar ("f"),
           ELam (PVar ("x"), EApp (EVar ("f"), EApp (EVar ("f"), EVar ("x")))))
  Result: (('_3 -> '_3) -> ('_3 -> '_3))
  $ cat << EOF | ./REPL.exe -
  > fun x -> let v = x in v
  > EOF
  parsing a string 'fun x -> let v = x in v'
  Parsed: ELam
          (PVar ("x"), ELet (NonRecursive, PVar ("v"), EVar ("x"), EVar ("v")))
  Result: ('_0 -> '_0)
  $ cat << EOF | ./REPL.exe -
  > let add = fun  x -> fun  y -> x + y in
  > let add1 = add 1 in
  > add1 13
  > EOF
  parsing a string 'let add = fun  x -> fun  y -> x + y in
  let add1 = add 1 in
  add1 13'
  Parsed: ELet
          (NonRecursive, PVar ("add"),
           ELam
           (PVar ("x"),
            ELam (PVar ("y"), EApp (EApp (EVar ("+"), EVar ("x")), EVar ("y")))),
           ELet
           (NonRecursive, PVar ("add1"), EApp (EVar ("add"), EConst (1)),
            EApp (EVar ("add1"), EConst (13))))
  Result: int
  $ cat << EOF | ./REPL.exe -
  > let add = fun x -> x + x in
  > let add1 = add 1 in
  > add 1
  > EOF
  parsing a string 'let add = fun x -> x + x in
  let add1 = add 1 in
  add 1'
  Parsed: ELet
          (NonRecursive, PVar ("add"),
           ELam (PVar ("x"), EApp (EApp (EVar ("+"), EVar ("x")), EVar ("x"))),
           ELet
           (NonRecursive, PVar ("add1"), EApp (EVar ("add"), EConst (1)),
            EApp (EVar ("add"), EConst (1))))
  Result: int
