type ident = string [@@deriving show]

type expr = EVar of ident | EConst of int | EBinop of string * expr * expr
[@@deriving show { with_path = false }]

type stmt =
  | Assgn of ident * expr
  | Ite of expr * stmt list * stmt list
  | While of expr * stmt list
[@@deriving show { with_path = false }]

type program = ident list * stmt list [@@deriving show { with_path = false }]

module String_set = Set.Make (String)

let all_vars =
  let acc = ref String_set.empty in
  let extend x = acc := String_set.add x !acc in
  let rec on_expr = function
    | EVar v -> extend v
    | EConst _ -> ()
    | EBinop (_, l, r) ->
        on_expr l;
        on_expr r
  in
  let rec on_stmt = function
    | Assgn (x, e) ->
        extend x;
        on_expr e
    | Ite (cond, th, el) ->
        on_expr cond;
        List.iter on_stmt th;
        List.iter on_stmt el
    | While (e, body) ->
        on_expr e;
        List.iter on_stmt body
  in

  fun p ->
    List.iter on_stmt p;
    !acc
