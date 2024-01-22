type reg =
  | SP
  | User of int
  | ROffset of reg * int
  | Temp_reg of int
  | RU of string

let rec pp_reg ppf =
  let open Format in
  function
  | SP -> fprintf ppf "sp"
  | RU s -> fprintf ppf "%s" s
  | Temp_reg n -> fprintf ppf "t%d" n
  | ROffset (r, 0) -> fprintf ppf "(%a)" pp_reg r
  | ROffset (r, n) -> fprintf ppf "%d(%a)" n pp_reg r
  | User _ -> failwith "not implemented (User _)"

type instr =
  | Addi of reg * reg * int
  | Add of reg * reg * reg
  | Sub of reg * reg * reg
  | Mulw of reg * reg * reg  (** RV64M *)
  | Li of reg * int
  | Ecall
  | Call of string
  | Ret
  | Lla of reg * string
  | Ld of reg * reg  (** [ld ra, (sp)] *)
  | Sd of reg * reg
  | Beq of reg * reg * string
  | Blt of reg * reg * string
  | Label of string
  | Comment of string

let pp_instr ppf =
  let open Format in
  function
  | Addi (r1, r2, n) -> fprintf ppf "addi %a, %a, %d" pp_reg r1 pp_reg r2 n
  | Add (r1, r2, r3) ->
      fprintf ppf "add  %a, %a, %a" pp_reg r1 pp_reg r2 pp_reg r3
  | Sub (r1, r2, r3) ->
      fprintf ppf "sub %a, %a, %a" pp_reg r1 pp_reg r2 pp_reg r3
  | Mulw (r1, r2, r3) ->
      fprintf ppf "mulw %a, %a, %a" pp_reg r1 pp_reg r2 pp_reg r3
  | Li (r, n) -> fprintf ppf "li %a, %d" pp_reg r n
  | Ecall -> fprintf ppf "ecall"
  | Call f -> fprintf ppf "call %s" f
  | Ret -> fprintf ppf "ret"
  | Lla (r1, s) -> fprintf ppf "lla %a, %s" pp_reg r1 s
  | Ld (r1, r2) -> fprintf ppf "ld %a, %a" pp_reg r1 pp_reg r2
  | Sd (r1, r2) -> fprintf ppf "sd %a, %a" pp_reg r1 pp_reg r2
  | Beq (r1, r2, offset) ->
      fprintf ppf "beq %a, %a, %s" pp_reg r1 pp_reg r2 offset
  | Blt (r1, r2, offset) ->
      fprintf ppf "blt %a, %a, %s" pp_reg r1 pp_reg r2 offset
  | Label s -> fprintf ppf "%s:" s
  | Comment s -> fprintf ppf "# %s" s
(* | _ -> failwith "Not implemented" *)

let addi k r1 r2 n = k @@ Addi (r1, r2, n)
let add k r1 r2 r3 = k @@ Add (r1, r2, r3)
let sub k r1 r2 r3 = k @@ Sub (r1, r2, r3)
let mulw k r1 r2 r3 = k @@ Mulw (r1, r2, r3)
let li k r n = k (Li (r, n))
let ecall k = k Ecall
let call k name = k (Call name)
let ret k = k Ret
let lla k r name = k (Lla (r, name))
let ld k a b = k (Ld (a, b))
let sd k a b = k (Sd (a, b))
let beq k r1 r2 r3 = k @@ Beq (r1, r2, r3)
let blt k r1 r2 r3 = k @@ Blt (r1, r2, r3)
let label k s = k (Label s)
let comment k s = k (Comment s)
(* TODO: add format    *)

(** Registers *)

let zero = RU "zero"
let ra = RU "ra"
let sp = SP
let a0 = RU "a0"
let a1 = RU "a1"
let t0 = Temp_reg 0
let t1 = Temp_reg 1
let t3 = Temp_reg 3
let t4 = Temp_reg 4
let t5 = Temp_reg 5

(** Emission and storage *)
let code : (instr * string) Queue.t = Queue.create ()

let rec flush_queue ppf =
  if Queue.is_empty code then ()
  else
    let open Format in
    let i, comm = Queue.pop code in
    (match i with
    | Comment "" -> ()
    | Comment s -> fprintf ppf "# %s\n%!" s
    | _ ->
        fprintf ppf "  %a" pp_instr i;
        if comm <> "" then fprintf ppf " # %s" comm;
        fprintf ppf "\n");
    flush_queue ppf

let emit ?(comm = "") instr = instr (fun i -> Queue.add (i, comm) code)

let __ () =
  let _x = emit addi SP SP 4 in
  ()
