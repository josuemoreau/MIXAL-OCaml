%token PLUS MINUS MUL DIV DIVP FSPEC LPAR RPAR
%token <string> MIXOP
%token <string> ASSOP
%token <string>ALFOP
%token <string> STR
%token <int> INT
%token ASTERISK
%token EQUAL COMMA EINSTR EOF
%start <Ast.ast> main
%%

main:
  i = instrs; EOF { i }
;

instrs:
| l = line                      { Ast.Line l         }
| l = line; EINSTR; is = instrs { Ast.Instrs (l, is) }
;

line:
| i = instr              { Ast.Instr i              }
| sym = STR; i = instr   { Ast.SymDefInstr (sym, i) }
;

instr:
| op = MIXOP; addr = apart; i = ipart; f = fpart {
    Ast.MixInstr { op = op; addr = addr; index = i; fspec = f }
  }
| op = ASSOP; addr = wpart { Ast.AssInstr { op = op; addr = addr } }
| s = ALFOP                { Ast.AlfInstr { value = s }            }
;

awpart:
  e = expr; f = fpart { Ast.WExpr (e, f) }
;

wpart:
| e = awpart                     { Ast.WAtomic e     }
| e1 = wpart; COMMA; e2 = awpart { Ast.WMul (e1, e2) }
;

fpart:
| epsilon              { Ast.FEmpty  }
| LPAR; e = expr; RPAR { Ast.FExpr e }
;

ipart:
| epsilon         { Ast.IEmpty  }
| COMMA; e = expr { Ast.IExpr e }
;

apart:
| epsilon                 { Ast.AEmpty     }
| e = expr                { Ast.AExpr e    }
(* | s = STR                 { AFuture s  } impossible à savoir *)
| EQUAL; w = wpart; EQUAL { Ast.ALiteral w }
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
| i = INT     { Ast.ENum i    }
| sym = STR   { Ast.ESym sym  }
| ASTERISK    { Ast.EAsterisk }
;

epsilon:
    { () }
