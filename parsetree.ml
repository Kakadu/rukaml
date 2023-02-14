type pattern =
  | PVar of string
  | PTuple of pattern * pattern * pattern list
[@@deriving show { with_path = false }]

let pvar s = PVar s

type rec_flag =
  | Recursive
  | NonRecursive
[@@deriving show { with_path = false }]

type expr =
  | EConst of int
  | EVar of string
  | EIf of expr * expr * expr
  | ELam of pattern * expr
  | EApp of expr * expr
  | ETuple of expr * expr * expr list
  | ELet of rec_flag * pattern * expr * expr
[@@deriving show { with_path = false }]

let econst n = EConst n
let evar s = EVar s
let elam v body = ELam (v, body)
let eapp1 f x = EApp (f, x)
let etuple a b xs = ETuple (a, b, xs)

let eapp f = function
  | [] -> f
  | args -> List.fold_left eapp1 f args
;;

let elet ?(isrec = NonRecursive) p b wher = ELet (isrec, p, b, wher)
let eite c t e = EIf (c, t, e)
let emul a b = eapp (evar "*") [ a; b ]
let eadd a b = eapp (evar "+") [ a; b ]
let esub a b = eapp (evar "-") [ a; b ]
let eeq a b = eapp (evar "=") [ a; b ]
let elt a b = eapp (evar "<") [ a; b ]
let egt a b = eapp (evar ">") [ a; b ]

type value_binding = rec_flag * pattern * expr
type structure_item = value_binding
type structure = structure_item list

let group_lams body =
  let rec helper acc = function
    | ELam (p, body) -> helper (p :: acc) body
    | not_a_lam -> List.rev acc, not_a_lam
  in
  helper [] body
;;
