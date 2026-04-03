  $ run () { ../../driver/driver.exe $1 --target typedtree -o a.ml && cat a.ml; }

  $ run << EOF
  > let test () =
  >   match 1, (), 1, (), '1' with
  >   | _, _, _, _, _ -> printf "test failed 2"
  >   | 1, (), 1, (), '1' -> printf "test failed 1"
  > EOF
  let test: unit -> unit =
    fun () -> match (1, (), 1, (), '1') with
                | (_, _, _ _ _) -> printf "test failed 2"
                | (1, (), 1 () 1) -> printf "test failed 1"


  $ run << EOF
  > let test () =
  >   match 1, (), true, "123", '1' with
  >   | 1, (), true, "234", '1' -> printf "test failed 1"
  >   | _, _, _, _, _ -> printf "test failed 2"
  > EOF
  let test: unit -> unit =
    fun () -> match (1, (), true, "123", '1') with
                | (1, (), true "234" 1) -> printf "test failed 1"
                | (_, _, _ _ _) -> printf "test failed 2"

  $ run << EOF
  > let test () =
  >   match (), 1, (), '1' with
  >   | (), 1, (), '1' -> printf "test failed 1"
  >   | _, _, _, _ -> printf "test failed 2"
  > EOF
  let test: unit -> unit =
    fun () -> match ((), 1, (), '1') with
                | ((), 1, () 1) -> printf "test failed 1"
                | (_, _, _ _) -> printf "test failed 2"

  $ run << EOF
  > let test () =
  >   match 1, (), '1' with
  >   | 1, (), '1' -> printf "test failed 1"
  >   | _, _, _ -> printf "test failed 2"
  > EOF
  let test: unit -> unit =
    fun () -> match (1, (), '1') with
                | (1, (), 1) -> printf "test failed 1"
                | (_, _, _) -> printf "test failed 2"
