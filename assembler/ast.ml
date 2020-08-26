(* type unop = UPlus | UMinus *)

type binop = BAdd | BSub | BMul | BDiv | BDivP | BFSpec

type atomicexpr =
  | ENum of int
  | ESym of string
  | EAsterisk
type expr =
  (* | EAtomic of atomicexpr *)
  | EPos of atomicexpr
  | ENeg of atomicexpr
  (* | EUnary of unop * atomicexpr *)
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
  (* | AFuture of string *)
  | ALiteral of wval

type mix_instr = {
    (* symdef : string option; *)
    op     : string;
    addr   : addr;
    index  : index;
    fspec  : fspec
}
type ass_instr = {
    (* symdef : string option; *)
    op     : string;
    addr   : wval
}
type alf_instr = {
    (* symdef : string option; *)
    value  : string
}

type instr =
  | MixInstr of mix_instr
  | AssInstr of ass_instr
  | AlfInstr of alf_instr

type line =
  | SymDefInstr of string * instr
  | Instr of instr

type ast =
  | Line of line
  | Instrs of line * ast

open Format

let pp_binop f = function
  | BAdd   -> printf "+"
  | BSub   -> printf "-"
  | BMul   -> printf "*"
  | BDiv   -> printf "/"
  | BDivP  -> printf "//"
  | BFSpec -> printf ":"

let pp_atomicexpr f = function
  | ENum i    -> printf "%d" i
  | ESym s    -> printf "%s" s
  | EAsterisk -> printf "*"

let rec pp_expr f = function
  | EPos a -> printf "%a" pp_atomicexpr a
  | ENeg a -> printf "-%a" pp_atomicexpr a
  | EBinary (op, e, a) -> printf "%a %a %a" pp_expr e pp_binop op pp_atomicexpr a

let pp_index f = function
  | IEmpty -> ()
  | IExpr e -> printf ", %a" pp_expr e

let pp_fspec f = function
  | FEmpty -> ()
  | FExpr e -> printf "(%a)" pp_expr e

let pp_atomicwval f = function
  | WExpr (e, f) -> printf "%a%a" pp_expr e pp_fspec f

let rec pp_wval f = function
  | WAtomic w -> printf "%a" pp_atomicwval w
  | WMul (e, w) -> printf "%a, %a" pp_wval e pp_atomicwval w

let pp_addr f = function
  | AEmpty -> ()
  | AExpr e -> printf "%a" pp_expr e
  | ALiteral w -> printf "=%a=" pp_wval w

let pp_mixinstr f (i: mix_instr) =
  printf "%s %a%a%a" i.op pp_addr i.addr pp_index i.index pp_fspec i.fspec

let pp_assinstr f (i: ass_instr) =
  printf "%s %a" i.op pp_wval i.addr

let pp_alfinstr f (i: alf_instr) =
  printf "alf %s" i.value

let pp_instr f = function
  | MixInstr i -> printf "%a" pp_mixinstr i
  | AssInstr i -> printf "%a" pp_assinstr i
  | AlfInstr i -> printf "%a" pp_alfinstr i

let pp_line f = function
  | SymDefInstr (s, i) -> printf "%s %a" s pp_instr i
  | Instr i            -> printf "%a" pp_instr i

let rec pp_ast f = function
  | Line l -> printf "%a" pp_line l
  | Instrs (l, s) -> printf "%a\n%a" pp_line l pp_ast s

let pp = printf "%a" pp_ast
