open Op
open Format

type binop = BAdd | BSub | BMul | BDiv | BDivP | BFSpec

type atomicexpr =
  | ENum of int
  | ESym of string
  | EAsterisk
type expr =
  | EPos of atomicexpr
  | ENeg of atomicexpr
  | EBinary of binop * expr * atomicexpr

type index = IEmpty | IExpr of expr

type fspec = FEmpty | FExpr of expr

type atomicwval = WExpr of expr * fspec
type wval =
  | WAtomic of atomicwval
  | WMul of wval * atomicwval

type addr =
  | AEmpty
  | AExpr of expr
  | ALiteral of wval

type mix_instr = {
    op     : mixop;
    addr   : addr;
    index  : index;
    fspec  : fspec
}
type ass_instr = {
    op     : assop;
    addr   : wval
}
type alf_instr = {
    value  : string
}

type instr =
  | MixInstr of mix_instr
  | AssInstr of ass_instr
  | AlfInstr of alf_instr

type line =
  | SymDefInstr of string * instr
  | Instr of instr

type ast = line list

let pp_binop f = function
  | BAdd   -> fprintf f "+"
  | BSub   -> fprintf f "-"
  | BMul   -> fprintf f "*"
  | BDiv   -> fprintf f "/"
  | BDivP  -> fprintf f "//"
  | BFSpec -> fprintf f ":"

let pp_atomicexpr f = function
  | ENum i    -> fprintf f "%d" i
  | ESym s    -> fprintf f "%s" s
  | EAsterisk -> fprintf f "*"

let rec pp_expr f = function
  | EPos a -> fprintf f "%a" pp_atomicexpr a
  | ENeg a -> fprintf f "-%a" pp_atomicexpr a
  | EBinary (op, e, a) -> fprintf f "%a %a %a" pp_expr e pp_binop op pp_atomicexpr a

let pp_index f = function
  | IEmpty -> ()
  | IExpr e -> fprintf f ", %a" pp_expr e

let pp_fspec f = function
  | FEmpty -> ()
  | FExpr e -> fprintf f "(%a)" pp_expr e

let pp_atomicwval f = function
  | WExpr (e, fs) -> fprintf f "%a%a" pp_expr e pp_fspec fs

let rec pp_wval f = function
  | WAtomic w -> fprintf f "%a" pp_atomicwval w
  | WMul (e, w) -> fprintf f "%a, %a" pp_wval e pp_atomicwval w

let pp_addr f = function
  | AEmpty -> ()
  | AExpr e -> fprintf f "%a" pp_expr e
  | ALiteral w -> fprintf f "=%a=" pp_wval w

let pp_mixinstr f (i: mix_instr) =
  fprintf f "%s %a%a%a" (str_of_mixop i.op) pp_addr i.addr pp_index i.index pp_fspec i.fspec

let pp_assinstr f (i: ass_instr) =
  fprintf f "%s %a" (str_of_assop i.op) pp_wval i.addr

let pp_alfinstr f (i: alf_instr) =
  fprintf f "alf %s" i.value

let pp_instr f = function
  | MixInstr i -> fprintf f "%a" pp_mixinstr i
  | AssInstr i -> fprintf f "%a" pp_assinstr i
  | AlfInstr i -> fprintf f "%a" pp_alfinstr i

let pp_line f = function
  | SymDefInstr (s, i) -> fprintf f "%s %a" s pp_instr i
  | Instr i            -> fprintf f "%a" pp_instr i

let pp_ast f t = List.iter (fun l -> fprintf f "%a\n" pp_line l) t

let print_ast = printf "%a" pp_ast
