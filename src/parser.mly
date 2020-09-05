%token PLUS MINUS MUL DIV DIVP FSPEC LPAR RPAR
%token <string> MIXOP
%token <string> ASSOP
%token <string> ALFOP
%token <string> IDENT
%token <int> LOCALSYMDEF
%token <int> LOCALSYMBEFORE
%token <int> LOCALSYMFORWARD
%token <int> INT
%token EQUAL COMMA EINSTR EOF
%start <Ast.ast> main
%%

main:
  i = instrs; EOF { i }
;

instrs:
  is = separated_list(EINSTR, line) { is }
;

line:
| i = instr              { Ast.Instr i                              }
| sym = IDENT; i = instr   { Ast.SymDefInstr (Ast.SymString sym, i)               }
| sym = LOCALSYMDEF; i = instr { Ast.SymDefInstr (Ast.SymLocal sym, i) }
;

instr:
| op = MIXOP; addr = apart; i = ipart; f = fpart {
    Ast.MixInstr { op = Op.mixop_of_str op; addr = addr; index = i; fspec = f }
  }
| op = ASSOP; addr = wpart { Ast.AssInstr { op = Op.assop_of_str op; addr = addr } }
| s = ALFOP                { Ast.AlfInstr { value = s }                            }
;

awpart:
  e = expr; f = fpart { Ast.WExpr (e, f) }
;

wpart:
| e = awpart                     { Ast.WAtomic e     }
| e1 = wpart; COMMA; e2 = awpart { Ast.WMul (e1, e2) }
;

fpart:
|                      { Ast.FEmpty  }
| LPAR; e = expr; RPAR { Ast.FExpr e }
;

ipart:
|                 { Ast.IEmpty  }
| COMMA; e = expr { Ast.IExpr e }
;

apart:
|                         { Ast.AEmpty                              }
| f = LOCALSYMFORWARD     { Ast.AExpr (Ast.EPos (Ast.ELocalSymF f)) }
| e = expr                { Ast.AExpr e                             }
| EQUAL; w = wpart; EQUAL { Ast.ALiteral w               }
;

expr:
| e = aexpr                    { Ast.EPos e                       }
| PLUS; e = aexpr              { Ast.EPos e                       }
| MINUS; e = aexpr             { Ast.ENeg e                       }
| e1 = expr; PLUS; e2 = aexpr  { Ast.EBinary (Ast.BAdd, e1, e2)   }
| e1 = expr; MINUS; e2 = aexpr { Ast.EBinary (Ast.BSub, e1, e2)   }
| e1 = expr; MUL; e2 = aexpr   { Ast.EBinary (Ast.BMul, e1, e2)   }
| e1 = expr; DIV; e2 = aexpr   { Ast.EBinary (Ast.BDiv, e1, e2)   }
| e1 = expr; DIVP; e2 = aexpr  { Ast.EBinary (Ast.BDivP, e1, e2)  }
| e1 = expr; FSPEC; e2 = aexpr { Ast.EBinary (Ast.BFSpec, e1, e2) }
;

aexpr:
| i = INT              { Ast.ENum i         }
| sym = LOCALSYMBEFORE { Ast.ELocalSymB sym }
| sym = IDENT          { Ast.ESym sym       }
| MUL                  { Ast.EAsterisk      }
;
