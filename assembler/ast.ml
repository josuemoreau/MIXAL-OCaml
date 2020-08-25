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

