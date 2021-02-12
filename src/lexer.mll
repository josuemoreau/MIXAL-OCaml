{
  open Format
  open Parser

  module StringSet = Set.Make(String)
  module StringMap = Map.Make(String)

  let ass_keywords = StringSet.of_list ["equ"; "orig"; "con"; "end"]
  let mix_keywords = StringSet.of_list [
      "nop";
      "add"; "sub"; "mul"; "div";
      "num"; "char"; "hlt";
      "sla"; "sra"; "slax"; "srax"; "slc"; "src";
      "move";
      "lda"; "ld1"; "ld2"; "ld3"; "ld4"; "ld5"; "ld6"; "ldx";
      "ldan"; "ld1n"; "ld2n"; "ld3n"; "ld4n"; "ld5n"; "ld6n"; "ldxn";
      "sta"; "st1"; "st2"; "st3"; "st4"; "st5"; "st6"; "stx"; "stj"; "stz";
      "jbus"; "ioc"; "in"; "out"; "jred";
      "jmp"; "jsj"; "jov"; "jnov"; "jl"; "je"; "jg"; "jge"; "jne"; "jle";
      "jan"; "jaz"; "jap"; "jann"; "janz"; "janp";
      "j1n"; "j1z"; "j1p"; "j1nn"; "j1nz"; "j1np";
      "j2n"; "j2z"; "j2p"; "j2nn"; "j2nz"; "j2np";
      "j3n"; "j3z"; "j3p"; "j3nn"; "j3nz"; "j3np";
      "j4n"; "j4z"; "j4p"; "j4nn"; "j4nz"; "j4np";
      "j5n"; "j5z"; "j5p"; "j5nn"; "j5nz"; "j5np";
      "j6n"; "j6z"; "j6p"; "j6nn"; "j6nz"; "j6np";
      "jxn"; "jxz"; "jxp"; "jxnn"; "jxnz"; "jxnp";
      "inca"; "deca"; "enta"; "enna";
      "inc1"; "dec1"; "ent1"; "enn1";
      "inc2"; "dec2"; "ent2"; "enn2";
      "inc3"; "dec3"; "ent3"; "enn3";
      "inc4"; "dec4"; "ent4"; "enn4";
      "inc5"; "dec5"; "ent5"; "enn5";
      "inc6"; "dec6"; "ent6"; "enn6";
      "incx"; "decx"; "entx"; "ennx";
      "cmpa"; "cmp1"; "cmp2"; "cmp3"; "cmp4"; "cmp5"; "cmp6"; "cmpx"
    ]

  let int_of_digit d = int_of_char d - int_of_char '0'

  let bol = ref true
}

let letters = ['a'-'z' 'A'-'Z']
let digits  = ['0'-'9']
let number  = digits+
let whitespaces = [' ' '\t' '\n']
let symbols = "+"|"-"|"*"|"/"|"//"|"="|","|"("|")"|":"

rule lexer = parse
  (* | [' ' '\t']* "**" [^'\n']* '\n' {
   *     Lexing.new_line lexbuf; lexer lexbuf
   *   } *)
  | whitespaces* '\n' whitespaces* {
      Lexing.new_line lexbuf; bol := true; EINSTR
    }
  | whitespaces+ {
      (* tous les espaces blancs ne contenant pas de retour à la ligne *)
      lexer lexbuf
    }
  | '+'  { bol := false; PLUS  }
  | '-'  { bol := false; MINUS }
  | '*'  { if !bol then comment lexbuf else begin bol := false; MUL end }
  | "//" { bol := false; DIVP  }
  | '/'  { bol := false; DIV   }
  | ':'  { bol := false; FSPEC }
  | ','  { bol := false; COMMA }
  | '('  { bol := false; LPAR  }
  | ')'  { bol := false; RPAR  }
  | '='  { bol := false; EQUAL }
  | ("alf "|"ALF ") ([^'\n']* as s) { bol := false; ALFOP s }
  | number as n   {
      bol := false;
      let nb = int_of_string n in
      INT nb
    }
  | (digits as i) ['H' 'h'] { bol := false; LOCALSYMDEF (int_of_digit i)     }
  | (digits as i) ['B' 'b'] { bol := false; LOCALSYMBEFORE (int_of_digit i)  }
  | (digits as i) ['F' 'f'] { bol := false; LOCALSYMFORWARD (int_of_digit i) }
  | (letters|digits|'_')+ as s {
      bol := false;
      let s' = String.lowercase_ascii s in
      if StringSet.mem s' ass_keywords then ASSOP s'
      else if StringSet.mem s' mix_keywords then MIXOP s'
      else IDENT s'
    }
  | eof { EOF }
  | _ { failwith "lexical error" }
and comment = parse
  | "\n" { Lexing.new_line lexbuf; lexer lexbuf }
  | _    { comment lexbuf }
{

}
