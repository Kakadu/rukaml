  $ minimlc -help
   MiniML compiler
    -dparsetree  dump parsetree
    -dtypedtree  dump typedtree
    - read from stdin
    -amd64  use amd64 backend
    -llvm  use llvm backend
    -help  Display this list of options
    --help  Display this list of options

# CPS Factorial
  $ cat << EOF | minimlc -dtypedtree -amd64 -
  > let rec fack n k =
  >  if n=1 then k 1 else fack (n-1) (fun m -> k (n*m))
  > 
  > let id2 x = x
  > let main = fack 6 id2
  > EOF
  let rec fack: (int -> ((int -> int) -> int)) =
    fun n k -> if (n = 1) then (k 1) else ((fack (n - 1)) fun m -> (k (n * m)))
  let id2: ('_1 -> '_1) =
    fun x -> x
  let main: int =
    ((fack 6) id2)
  $ cat a.out
  ; generated code for amd64
