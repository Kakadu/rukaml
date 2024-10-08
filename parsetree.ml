type pattern =
  | PVar of string
  | PTuple of pattern * pattern * pattern list
[@@deriving show { with_path = false }]

let pvar s = PVar s

type rec_flag =
  | Recursive
  | NonRecursive
[@@deriving show { with_path = false }]

type const =
  | PConst_int of int
  (* | PConst_string of string *)
  | PConst_bool of bool
[@@deriving show { with_path = false }]

type expr =
  | EUnit
  | EConst of const
  | EVar of string
  | EIf of expr * expr * expr
  | ELam of pattern * expr
  | EApp of expr * expr
  | ETuple of expr * expr * expr list
  | ELet of rec_flag * pattern * expr * expr
[@@deriving show { with_path = false }]

let const_int n = PConst_int n
let const_bool b = PConst_bool b
let eunit = EUnit
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
let ele a b = eapp (evar "<=") [ a; b ]
let egt a b = eapp (evar ">") [ a; b ]

type value_binding = rec_flag * pattern * expr [@@deriving show { with_path = false }]
type structure_item = value_binding [@@deriving show { with_path = false }]
type structure = structure_item list [@@deriving show { with_path = false }]

let group_lams body =
  let rec helper acc = function
    | ELam (p, body) -> helper (p :: acc) body
    | not_a_lam -> List.rev acc, not_a_lam
  in
  helper [] body
;;
