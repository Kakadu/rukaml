type item =
  { name : string
  ; arity : int
  ; typ : Miniml.Typedtree.scheme
  }

type info = item list

let output file xs =
  let _ : info = xs in
  Out_channel.with_open_bin file (fun ch -> Marshal.to_channel ch xs [])
;;

let from_file file = In_channel.with_open_bin file Marshal.from_channel

module SS = Map.Make (String)

type repr = item SS.t

let to_repr : info list -> repr =
  List.fold_left (List.fold_left (fun acc item -> SS.add item.name item acc)) SS.empty
;;

let find_exn : string -> repr -> item = SS.find
let length = SS.cardinal
