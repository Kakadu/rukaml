  $ cat << EOF | ./REPL.exe -
  > let main =
  >   let id x = x in
  >   id 0
  > EOF
  Parsed: let main = let id x = x in id 0
  After CCovv.
  let id x = x
  let main = id 0
