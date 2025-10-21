(* These files are extracted from library file_path (MIT License)
  to reduce number of dependencies
*)

module Path_string : sig
  (** Calls [if_some] with all parts of the given path but the final one, or [if_none] if
      given the root path or a relative path of a single part. *)
  val dirname : string -> if_some:(string -> 'a) -> if_none:(string -> 'a) -> 'a

  val is_absolute : string -> bool

  val root : string
end = struct
  let slash_char = '/'
  let slash_string = "/"
  let root = slash_string
  let root_length = String.length root

  let char_is_slash c = Char.equal c slash_char
  let is_root string = String.equal string root

  let is_absolute string = String.starts_with string ~prefix:root
  let is_relative string = not (is_absolute string)

  (* A helper for finding first or last slash characters. Returns -1 on failure. *)
  let rec find_slash string ~pos ~stop_at ~increment =
    if pos = stop_at
    then -1
    else if char_is_slash (String.unsafe_get string pos)
    then pos
    else find_slash string ~pos:(pos + increment) ~stop_at ~increment
  ;;

  let final_slash_index string =
    find_slash string ~pos:(String.length string - 1) ~stop_at:(-1) ~increment:(-1)
  ;;
  let dirname_at string ~non_negative_final_slash_index:index =
    if index = 0 then root else Base.String.prefix string index
  ;;
  let dirname string ~if_some ~if_none =
    if String.equal string root
    then if_none string
    else (
      let index = final_slash_index string in
      if index < 0
      then if_none string
      else if_some (dirname_at string ~non_negative_final_slash_index:index))
  ;;
end

module Path : sig
  type t
  val dot_dot : t
  val dot : t

  val to_string : t -> string
  val of_string : string -> t
  val append_part : t -> string -> t
  val append : t -> t -> t
  val to_parts : t -> string list
  val chop_prefix_if_exists : t -> prefix:t -> t
  val dirname_defaulting_to_dot : t -> t
end = struct
  type t = string
  let to_string s = s
  let of_string s = s
  let append_part path part = path ^ Stdlib.Filename.dir_sep ^ part

  let to_parts = String.split_on_char '/'
  let chop_prefix_if_exists s ~prefix =
    if Base.String.is_prefix s ~prefix
    then
      s
      |> Base.String.chop_prefix_if_exists ~prefix
      |> Base.String.chop_prefix_if_exists ~prefix:"/"
    else s
  ;;
  let append = append_part
  let dot_dot = ".."
  let dot = "."

  let dirname_defaulting_to_dot t =
    Path_string.dirname (to_string t) ~if_some:Stdlib.Fun.id ~if_none:(fun _ -> dot)
  ;;
end
