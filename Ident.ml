type t =
  { id : int
  ; hum_name : string
  }

type ident = t

let id_counter = ref 0

let of_string hum_name =
  incr id_counter;
  { id = !id_counter; hum_name }
;;

let equal left { id; _ } = left.id = id
let compare left { id; _ } = Int.compare left.id id
let pp ppf { hum_name; _ } = Format.fprintf ppf "%s" hum_name

module Id_map = Map.Make (struct
    type nonrec t = t

    let compare = compare
  end)

module String_map = Map.Make (String)

module Ident_map : sig
  type +'a t

  val empty : 'a t
  val add : string -> ident -> 'a -> 'a t -> 'a t
  val ident_of_string_exn : string -> _ t -> ident
  val find_by_string_exn : string -> 'a t -> 'a
  val find_by_ident : ident -> 'a t -> 'a
  val fold_idents : f:('acc -> ident * 'b -> 'acc) -> init:'acc -> 'b t -> 'acc
  val iter_idents : f:(ident -> 'a -> unit) -> 'a t -> unit
  val map : f:('a -> 'b) -> 'a t -> 'b t
end = struct
  type nonrec 'a t = 'a Id_map.t * t String_map.t

  let empty = Id_map.empty, String_map.empty
  let ident_of_string_exn name (_, m) = String_map.find name m

  let add str ident v (left, right) =
    Id_map.add ident v left, String_map.add str ident right
  ;;

  let find_by_ident id (left, _) = Id_map.find id left

  let find_by_string_exn str (left, s_to_i) =
    let id = String_map.find str s_to_i in
    Id_map.find id left
  ;;

  let fold_idents ~f ~init (left, _) = Id_map.fold (fun k v acc -> f acc (k, v)) left init
  let iter_idents ~f (left, _) = Id_map.iter f left
  let map ~f (idents, names) = Id_map.map f idents, names
end
