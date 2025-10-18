type pattern =
  | PAny
  | PVar of string
  | PTuple of pattern * pattern * pattern list
  | PConstruct of string * pattern option
[@@deriving show { with_path = false }]

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
  | EConstruct of string * expr option
  | EMatch of expr * (pattern * expr) list1
[@@deriving show { with_path = false }]

and 'a list1 = 'a * 'a list [@@deriving show { with_path = false }]

let evar s = EVar s
let pvar s = PVar s
let const_int n = PConst_int n
let const_bool b = PConst_bool b
let eunit = EUnit
let econst n = EConst n
let elam v body = ELam (v, body)
let eapp1 f x = EApp (f, x)
let etuple a b xs = ETuple (a, b, xs)
let ematch e pe pes = EMatch (e, (pe, pes))

let eapp f ?(is_right_assoc = false) args =
  match is_right_assoc, args with
  | _, [] -> f
  | false, args -> List.fold_left eapp1 f args
  | true, args -> List.fold_right eapp1 args f
;;

let pnil = PConstruct ("Nil", None)
let enil = EConstruct ("Nil", None)
let pcons hd tl = PConstruct ("Cons", Some (PTuple (hd, tl, [])))
let econs hd tl = EConstruct ("Cons", Some (ETuple (hd, tl, [])))
let elet ?(isrec = NonRecursive) p b wher = ELet (isrec, p, b, wher)
let eite c t e = EIf (c, t, e)
let emul a b = eapp (evar "*") [ a; b ]
let eadd a b = eapp (evar "+") [ a; b ]
let esub a b = eapp (evar "-") [ a; b ]
let eeq a b = eapp (evar "=") [ a; b ]
let elt a b = eapp (evar "<") [ a; b ]
let ele a b = eapp (evar "<=") [ a; b ]
let egt a b = eapp (evar ">") [ a; b ]
let e_cons a b = eapp ~is_right_assoc:true (evar "::") [ a; b ]

type value_binding = rec_flag * pattern * expr [@@deriving show { with_path = false }]

type type_declaration =
  { typedef_params : string list (** ['a] is param in [type 'a list = ...]  *)
  ; typedef_name : string (** [list] is name in [type 'a list = ...]  *)
  ; typedef_kind : type_kind
  }
[@@deriving show { with_path = false }]

and type_kind =
  | KAbstract of core_type option (** [ type t ], [ type t = x ] *)
  | KVariants of (string * core_type option) list1 (** [ type t = Some of int | None ]  *)
[@@deriving show { with_path = false }]

and core_type =
  | CTVar of string (** [ 'a, 'b ] are type variables in [ type ('a, 'b) ty = ... ] *)
  | CTArrow of core_type * core_type (** ['a -> 'b] *)
  | CTTuple of core_type * core_type * core_type list (** [ 'a * 'b * 'c ] *)
  | CTConstr of string * core_type list (** [ int ], ['a option], [ ('a, 'b) list ] *)
[@@deriving show { with_path = false }]

type structure_item =
  | SValue of value_binding (** [ let x = ... ] *)
  | SType of type_declaration list1 (** [ type x = ... ] *)
[@@deriving show { with_path = false }]

type structure = structure_item list [@@deriving show { with_path = false }]

let group_lams body =
  let rec helper acc = function
    | ELam (p, body) -> helper (p :: acc) body
    | not_a_lam -> List.rev acc, not_a_lam
  in
  helper [] body
;;
