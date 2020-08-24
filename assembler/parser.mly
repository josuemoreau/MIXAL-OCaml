%{
  (* type unop = UPlus | UMinus *)

  type binop = BAdd | BSub | BMul | BDiv | BDivP | BFSpec

  type atomicexpr =
    | ENum of int
    | ESym of string
    | EAsterisk
  type expr =
    (* | EAtomic of atomicexpr *)
    | EPos of atomicexpr
    | ENef of atomicexpr
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
    | AFuture of string
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
%}
%token PLUS MINUS MUL DIV DIVP FSPEC LPAR RPAR
%token <string> MIXOP
%token <string> ASSOP
%token ALFOP
%token <string> STR
%token <int> INT
%token ASTERISK
%token COMMA EOF
%start <ast> main
%%

main:
  i = instrs; EOF { i }
;

instrs:
| l = line              { Line l         }
| l = line; is = instrs { Instrs (l, is) }
;

line:
| i = instr              { Instr i              }
| sym = STR; i = instr   { SymDefInstr (sym, i) }
;

instr:
| op = MIXOP; addr = apart; i = ipart; f = fpart {
    MixInstr { op = op; addr = addr; index = i; fspec = f }
  }
| op = ASSOP; addr = wpart { AssInstr { op = op; addr = addr } }
| op = ALFOP; s = STR      { AlfInstr { value = s }            }
;

awpart:
  e = expr; f = fpart { WExpr (e, f) }
;

wpart:
| e = awpart                     { WAtomic e     }
| e1 = wpart; COMMA; e2 = awpart { WMul (e1, e2) }
;

fpart:
| epsilon              { FEmpty  }
| LPAR; e = expr; RPAR { FExpr e }
;

ipart:
| epsilon         { IEmpty  }
| COMMA; e = expr { IExpr e }
;

apart:
| epsilon   { AEmpty     }
| e = expr  { AExpr e    }
| s = STR   { AFuture s  }
| w = wpart { ALiteral w }
;

expr:
| e = aexpr                    { EPos e                   }
| PLUS; e = aexpr              { EPos e                   }
| MINUS; e = aexpr             { ENeg e                   }
| e1 = expr; PLUS; e2 = aexpr  { EBinary (BAdd, e1, e2)   }
| e1 = expr; MINUS; e2 = aexpr { EBinary (BSub, e1, e2)   }
| e1 = expr; MUL; e2 = aexpr   { EBinary (BMul, e1, e2)   }
| e1 = expr; DIV; e2 = aexpr   { EBinary (BDiv, e1, e2)   }
| e1 = expr; DIVP; e2 = aexpr  { EBinary (BDivP, e1, e2)  }
| e1 = expr; FSPEC; e2 = aexpr { EBinary (BFspec, e1, e2) }
;

aexpr:
| i = INT     { ENum i    }
| sym = STR   { ESym sym  }
| ASTERISK    { EAsterisk }
;

epsilon:
    { () }
