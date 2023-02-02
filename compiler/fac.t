  $ cat << EOF | ../REPL.exe -
  > print 42
  > EOF
  parsing a string 'print 42'
  Parsed: EApp (EVar ("print"), EConst (42))
  Result: unit
  $ cat << EOF | ../REPL.exe -
  > let rec fac = fun n -> if n=1 then n else n * (fac (n-1)) in
  > 4
  > EOF
  parsing a string 'let rec fac = fun n -> if n=1 then n else n * (fac (n-1)) in
  4'
  Parsed: ELet
          (Recursive, PVar ("fac"),
           ELam
           (PVar ("n"),
            EIf
            (EApp (EApp (EVar ("="), EVar ("n")), EConst (1)), EVar ("n"),
             EApp
             (EApp (EVar ("*"), EVar ("n")),
              EApp
              (EVar ("fac"), EApp (EApp (EVar ("-"), EVar ("n")), EConst (1)))))),
           EConst (4))
  Result: int
  $ cat << EOF | ./compiler.exe
  > let rec zed f x = f (zed f) x in
  > let fac = fun self -> fun n -> if n=1 then 1 else n * (self (n-1)) in
  > zed fac
  > EOF
  compiler
  parsing a string 'let rec zed f x = f (zed f) x in
  let fac = fun self -> fun n -> if n=1 then 1 else n * (self (n-1)) in
  zed fac'
  Assembling '/tmp/file.il' , no listing file, to exe --> '/tmp/file.exe'
  
  /tmp/file.il : Warning -- Reference to undeclared extern assembly 'mscorlib', adding.
  Operation completed successfully
