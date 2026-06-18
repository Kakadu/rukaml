(*
test
  (targets amd64 rv64)
  (run (stdout "(1, 2, 3, 4, 5)"))
*)

let main =
  match ((((1, 2), 3), 4), 5) with
  | t, x5 ->
    match t with
    | t, x4 ->
      match t with
      | t, x3 ->
        match t with
        | x1, x2 ->
          let t =
            printf "(%d, %d, %d, %d, %d)" x1 x2 x3 x4 x5
          in
          0
