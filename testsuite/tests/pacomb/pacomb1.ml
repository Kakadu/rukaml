(*
test
  (targets rv32 rv64)
  (run (stdout "Success 'factrec1' on pos 8" "Success 'factrec1' on pos 0"))
*)

(* [begin skip] *)

(* provides compability with ocamlopt (rukaml parser skips this block) *)

let string_of_char_list chs = String.of_seq (List.to_seq chs)
let string_len s = String.length s
let string_nth s n = s.[n]
let string_equal = String.equal
let char_code = Char.code
let printf, fprintf, sprintf = Stdlib.Printf.(printf, fprintf, sprintf)
let array_get = Array.get
let array_set = Array.set
let array_len = Array.length
let list_length = List.length
let sys_argv = Sys.argv

let end_of_input ic =
  match input_char ic with
  | exception End_of_file -> true
  | c ->
    let () = seek_in ic (pos_in ic - 1) in
    false
;;

(* [end skip] *)

let failwith msg =
  let () = fprintf stderr "[compiler] error: %s \n" msg in
  exit 1
;;

(* list primitives *)

let list_rev ls =
  let rec aux ls acc =
    match ls with
    | [] -> acc
    | x :: xs -> aux xs (x :: acc)
  in
  aux ls []
;;

let rec list_fold f ls init =
  match ls with
  | [] -> init
  | x :: xs -> list_fold f xs (f init x)
;;

let rec list_fold_right f ls init =
  match ls with
  | [] -> init
  | x :: xs -> f x (list_fold_right f xs init)
;;

let rec list_map f ls =
  match ls with
  | [] -> []
  | x :: xs -> f x :: list_map f xs
;;

let rec list_iter f ls =
  match ls with
  | [] -> ()
  | x :: xs ->
    let () = f x in
    list_iter f xs
;;

let rec list_length ls =
  match ls with
  | [] -> 0
  | _ :: xs -> 1 + list_length xs
;;

let is_lowercase ch = char_code 'a' <= char_code ch && char_code ch <= char_code 'z'
let is_uppercase ch = char_code 'A' <= char_code ch && char_code ch <= char_code 'Z'
let is_digit ch = char_code '0' <= char_code ch && char_code ch <= char_code '9'
let is_alphanum ch =
  (* let () = printf "is_alphanum? %c\n" ch in *)
  is_lowercase ch || is_digit ch
let int_of_digit ch = char_code ch - char_code '0'

(* let int_of_digits chs =
  let rec pow b e = if e < 1 then 1 else b * pow b (e - 1) in
  let rez, _ =
    list_fold_right
      (fun digit (acc, pos) -> acc + (pow 10 pos * int_of_digit digit), pos + 1)
      chs
      (0, 0)
  in
  rez
;; *)

let is_cpp_keyword s =
  match s with
  | "int" -> true
  | "void" -> true
  | "if" -> true
  | "else" -> true
  | "for" -> true
  | "while" -> true
  | _ -> false
;;

(* parser combinators *)

type parsing_error =
  | Perr_message of string
  | Perr_expected of string
  | Perr_unexpected_eof

type parser_state = string * int

type 'a parsing_result =
  | Prez_success of 'a * parser_state
  | Prez_error of parsing_error

type 'a parser = parser_state -> 'a parsing_result

let return x state = Prez_success (x, state)
let fail err _state = Prez_error err

let bind p f state =

  match p state with
  | Prez_error err -> Prez_error err
  | Prez_success (x, state2) ->
      f x state2
;;

let map p f state =
  match p state with
  | Prez_error err -> Prez_error err
  | Prez_success (rez, state) -> return (f rez) state
;;

let take_while pred state =
  let rec aux state acc =
    let (str, pos) = state in

      if pos >= string_len str
      then return (list_rev acc) (str, pos)
      else (
        let ch = string_nth str pos in
        if pred ch
        then aux (str, pos + 1) (ch :: acc)
        else
          return (list_rev acc) (str, pos))
  in
  aux state []
;;

(* let take_while1 pred (str, pos) =
  if pos >= string_len str
  then Prez_error Perr_unexpected_eof
  else (
    let ch1 = string_nth str pos in
    if pred ch1
    then map (take_while pred) (fun chs ->
      ch1 :: chs) (str, pos + 1)
    else Prez_error (Perr_message "take_while1"))
;; *)

let take_while1_another pred (str, pos) =
  let rec loop cur =
    if cur >= string_len str
    then
      if (cur>pos) then Prez_success (substring str pos (cur-pos), (str,pos))
      else Prez_error (Perr_message "take_while1")
    else(
    let ch1 = string_nth str cur in
    if pred ch1
      then loop (cur+1)
    else
      if (cur>pos) then
        let  l = cur - pos in
        let news = substring str pos l in
        (* let () = trace_rukaml_val news in *)
        Prez_success (news, (str,pos))
      else Prez_error (Perr_message "take_while1")
    )
  in
  loop pos

(* let parse_identifier eta =
  bind (take_while1 is_alphanum) (fun chs ->
    let l = list_length chs in
    let() = printf "list len = %d\n" l in
    let name = string_of_char_list chs in
    if is_cpp_keyword name
    then fail (Perr_message "keyword as identifier")
    else if is_digit (string_nth name 0)
    then fail (Perr_expected "identifier")
    else return name) eta
;; *)

let parse_identifier2 eta =
  bind (take_while1_another is_alphanum) (fun name ->
    if is_digit (string_nth name 0)
    then fail (Perr_expected "identifier")
    else return name) eta
;;



let pp_parsing_error oc err =
  match err with
  | Perr_message msg -> fprintf oc "error: %s" msg
  | Perr_expected x -> fprintf oc "expected: %s" x
  | Perr_unexpected_eof -> fprintf oc "unexpected eof"
;;

let test5 () =
  let input = "factrec1(){}" in
  match take_while is_alphanum (input,0) with
  | Prez_error _ -> printf "Failed\n"
  | Prez_success (chs,(_,pos)) ->
      let name = string_of_char_list chs in
      printf "Success '%s' on pos %d\n" name   pos

let test6 () =
  let input = "factrec1(){}" in
  match take_while1_another is_alphanum (input,0) with
  | Prez_error _ -> printf "Failed\n"
  | Prez_success (name,(_,pos)) ->
      printf "Success '%s' on pos %d\n" name pos


(* main *)

let main =
  let () = test5 () in
  let () = test6 () in
  0
;;
