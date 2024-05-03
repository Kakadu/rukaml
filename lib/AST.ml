type ident = string [@@deriving show]

type stmt =
  | Assgn of ident * expr
  | Ite of expr * stmt list * stmt list
  | While of expr * stmt list

and expr = EVar of ident | EConst of int | EBinop of string * expr * expr
[@@deriving show { with_path = false }]

type program = ident list * stmt list [@@deriving show { with_path = false }]
