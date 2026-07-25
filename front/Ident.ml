type config = { mutable verbose : bool }

let config = { verbose = false }
let set_verbose verbose = config.verbose <- verbose

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

(* to create idents for constructors with the specified id *)
let ident hum_name id = { hum_name; id }
let equal left { id; _ } = left.id = id
let compare left { id; _ } = Int.compare left.id id

let pp ppf { hum_name; id } =
  if config.verbose
  then Format.fprintf ppf "%s/%d" hum_name id
  else Format.fprintf ppf "%s" hum_name
;;

let to_string = Format.asprintf "%a" pp

module Id_map = Map.Make (struct
    type nonrec t = t

    let compare = compare
  end)

module String_map = struct
  include Map.Make (String)

  let pp _f ppf _ = Format.fprintf ppf "?"
end

module Ident_map : sig
  type +'a t

  val empty : 'a t
  val add : string -> ident -> 'a -> 'a t -> 'a t
  val ident_of_string : string -> _ t -> ident
  val find_by_string : string -> 'a t -> 'a
  val find_by_string_opt : string -> 'a t -> 'a option
  val find_by_ident : ident -> 'a t -> 'a
  val find_by_ident_opt : ident -> 'a t -> 'a option
  val fold_idents : f:('acc -> ident * 'b -> 'acc) -> init:'acc -> 'b t -> 'acc
  val iter_idents : f:(ident -> 'a -> unit) -> 'a t -> unit
  val map : f:('a -> 'b) -> 'a t -> 'b t
end = struct
  type nonrec 'a t = 'a Id_map.t * t String_map.t

  let empty = Id_map.empty, String_map.empty
  let ident_of_string name (_, m) = String_map.find name m

  let add str ident v (left, right) =
    Id_map.add ident v left, String_map.add str ident right
  ;;

  let find_by_ident id (left, _) = Id_map.find id left
  let find_by_ident_opt id (left, _) = Id_map.find_opt id left

  let find_by_string str (left, s_to_i) =
    let id = String_map.find str s_to_i in
    Id_map.find id left
  ;;

  let find_by_string_opt str (left, s_to_i) =
    match String_map.find_opt str s_to_i with
    | None -> None
    | Some id -> Id_map.find_opt id left
  ;;

  let fold_idents ~f ~init (left, _) = Id_map.fold (fun k v acc -> f acc (k, v)) left init
  let iter_idents ~f (left, _) = Id_map.iter f left
  let map ~f (idents, names) = Id_map.map f idents, names
end

let concat_str idents =
  let open Format in
  asprintf "%a" (pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf ", ") pp) idents
;;

module Ident_set = struct
  include Set.Make (struct
      type nonrec t = t

      let compare = compare
    end)

  let to_string set = fold (fun id acc -> Format.asprintf "%a%s" pp id acc) set ""
  let to_list set = fold List.cons set []
end
