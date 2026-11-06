type const =
  | PConst_int of int
  | PConst_char of char
  | PConst_bool of bool
  | PConst_string of string
[@@deriving show { with_path = false }]

type pattern =
  | PUnit
  | PConst of const
  | PAny
  | PVar of string
  | PTuple of pattern * pattern * pattern list
  | PConstruct of string * pattern list
[@@deriving show { with_path = false }]

type rec_flag =
  | Recursive
  | NonRecursive
[@@deriving show { with_path = false }]

type expr =
  | EUnit
  | EArray of expr list
  | EConst of const
  | EVar of string
  | EIf of expr * expr * expr
  | ELam of pattern * expr
  | EApp of expr * expr
  | ETuple of expr * expr * expr list
  | ELet of rec_flag * pattern * expr * expr
  | EConstruct of string * expr list
  | EMatch of expr * (pattern * expr) list1
[@@deriving show { with_path = false }]

and 'a list1 = 'a * 'a list [@@deriving show { with_path = false }]

let evar s = EVar s
let pvar s = PVar s
let const_int n = PConst_int n
let const_char c = PConst_char c
let const_bool b = PConst_bool b
let const_string s = PConst_string s
let eunit = EUnit
let econst n = EConst n
let elam v body = ELam (v, body)
let eapp1 f x = EApp (f, x)
let etuple a b xs = ETuple (a, b, xs)
let ematch e pe pes = EMatch (e, (pe, pes))
let econstruct name args = EConstruct (name, args)
let pconstruct name args = PConstruct (name, args)
let earray xs = EArray xs

let eapp f ?(is_right_assoc = false) args =
  match is_right_assoc, args with
  | _, [] -> f
  | false, args -> List.fold_left eapp1 f args
  | true, args -> List.fold_right eapp1 args f
;;

let pnil = PConstruct ("[]", [])
let enil = EConstruct ("[]", [])
let pcons hd tl = PConstruct ("::", [ hd; tl ])
let econs hd tl = EConstruct ("::", [ hd; tl ])
let elet ?(isrec = NonRecursive) p b wher = ELet (isrec, p, b, wher)
let eite c t e = EIf (c, t, e)
let emul a b = eapp (evar "*") [ a; b ]
let ediv a b = eapp (evar "/") [ a; b ]
let eadd a b = eapp (evar "+") [ a; b ]
let esub a b = eapp (evar "-") [ a; b ]
let eeq a b = eapp (evar "=") [ a; b ]
let ene a b = eapp (evar "<>") [ a; b ]
let elt a b = eapp (evar "<") [ a; b ]
let ele a b = eapp (evar "<=") [ a; b ]
let ege a b = eapp (evar ">=") [ a; b ]
let egt a b = eapp (evar ">") [ a; b ]
let elor a b = eapp (evar "||") [ a; b ]
let eland a b = eapp (evar "&&") [ a; b ]
let e_cons a b = eapp ~is_right_assoc:true (evar "::") [ a; b ]

type value_binding = pattern * expr [@@deriving show { with_path = false }]

type type_declaration =
  { pty_params : string list (** ['a] is param in [type 'a list = ...]  *)
  ; pty_name : string (** [list] is name in [type 'a list = ...]  *)
  ; pty_kind : type_kind
  ; pty_manifest : core_type option
  }
[@@deriving show { with_path = false }]

and type_kind =
  | Ptype_abstract (** [ type t = int * bool ] *)
  | Ptype_variant of (string * core_type list) list1
  (** [ type t = Some of int | None ]  *)
[@@deriving show { with_path = false }]

and core_type =
  | Ptyp_var of string (** [ 'a, 'b ] are type variables in [ type ('a, 'b) ty = ... ] *)
  | Ptyp_arrow of core_type * core_type (** ['a -> 'b] *)
  | Ptyp_tuple of core_type * core_type * core_type list (** [ 'a * 'b * 'c ] *)
  | Ptyp_constr of string * core_type list (** [ int ], ['a option], [ ('a, 'b) list ] *)
[@@deriving show { with_path = false }]

type structure_item =
  | Pstr_value of rec_flag * value_binding * value_binding list (** [ let x = ... ] *)
  | Pstr_type of type_declaration list1 (** [ type x = ... ] *)
[@@deriving show { with_path = false }]

type structure = structure_item list [@@deriving show { with_path = false }]

let group_lams body =
  let rec helper acc = function
    | ELam (p, body) -> helper (p :: acc) body
    | not_a_lam -> List.rev acc, not_a_lam
  in
  helper [] body
;;

let is_recursive_vb (flg, _, _) =
  match flg with
  | Recursive -> true
  | NonRecursive -> false
;;

let pstr_value_of_vbs flg vbs =
  match vbs with
  | [] -> failwith "Bad argument of pstr_value_of_vbs"
  | h :: tl -> Pstr_value (flg, h, tl)
;;

type expr_mapper =
  { earray : expr_mapper -> expr list -> expr
  ; econst : expr_mapper -> const -> expr
  ; evar : expr_mapper -> string -> expr
  ; eif : expr_mapper -> expr -> expr -> expr -> expr
  ; elam : expr_mapper -> pattern -> expr -> expr
  ; eapp : expr_mapper -> expr -> expr -> expr
  ; etuple : expr_mapper -> expr -> expr -> expr list -> expr
  ; elet : expr_mapper -> rec_flag -> pattern -> expr -> expr -> expr
  ; econstruct : expr_mapper -> string -> expr list -> expr
  ; ematch : expr_mapper -> expr -> (pattern * expr) list1 -> expr
  ; expr : expr_mapper -> expr -> expr
  }

let default_mapper =
  { earray = (fun self xs -> EArray (List.map (self.expr self) xs))
  ; econst = (fun _ c -> econst c)
  ; evar = (fun _ -> evar)
  ; eif =
      (fun self c th el ->
        eite (self.expr self c) (self.expr self th) (self.expr self el))
  ; elam = (fun self p e -> elam p (self.expr self e))
  ; eapp = (fun self f x -> eapp1 (self.expr self f) (self.expr self x))
  ; etuple =
      (fun self a b xs ->
        etuple (self.expr self a) (self.expr self b) (List.map (self.expr self) xs))
  ; elet =
      (fun self isrec p b rhs -> elet ~isrec p (self.expr self b) (self.expr self rhs))
  ; econstruct = (fun self name xs -> econstruct name (List.map (self.expr self) xs))
  ; ematch =
      (fun self scru (h, tl) ->
        let f (p, e1) = p, self.expr self e1 in
        ematch (self.expr self scru) (f h) (List.map f tl))
  ; expr =
      (fun self -> function
         | EUnit -> EUnit
         | EArray xs -> self.earray self xs
         | EConst c -> self.econst self c
         | EVar v -> self.evar self v
         | EIf (c, th, el) -> self.eif self c th el
         | ELam (p, b) -> self.elam self p b
         | EApp (f, x) -> self.eapp self f x
         | ETuple (a, b, cs) -> self.etuple self a b cs
         | ELet (flg, p, b, wher) -> self.elet self flg p b wher
         | EConstruct (s, args) -> self.econstruct self s args
         | EMatch (scru, pats) -> self.ematch self scru pats)
  }
;;
