{
  open Format

  module StringSet = Set.Make(String)

  type tokens =
    | Tident of string
    | TassKeyword of string
    | TmixKeyword of string
    | Tsymbol of char
    | Tconst of int
    | Talf of string
    | Teof

  let ass_keywords = StringSet.of_list ["equ"; "orig"; "con"; "alf"; "end"]
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

  let filename = Sys.argv.(1)

  let pp f = function
    | Tident s      -> printf "IDENT(%s)" s
    | TassKeyword s -> printf "ASS(%s)" s
    | TmixKeyword s -> printf "MIX(%s)" s
    | Tsymbol c -> printf "SYM(%c)" c
    | Tconst n -> printf "CONST(%d)" n
    | Talf s   -> printf "ALF(%s)" s
    | Teof     -> ()

  let print_token = printf "%a " pp
}

let letters = ['a'-'z' 'A'-'Z']
let digits  = ['0'-'9']
let number  = digits+
let whitespaces = [' ' '\t' '\n']
let symbols = ['+' '-' '*' '/' '=' ',' '(' ')' ':']

rule assemble = parse
  | whitespaces* '*' [^'\n']* { assemble_rec lexbuf }
  | _ { assemble_rec lexbuf }
and assemble_rec = parse
  | '\n' whitespaces* '*' [^'\n']* '\n' { assemble_rec lexbuf }
  | whitespaces* '\n' whitespaces* { printf "\n"; assemble_rec lexbuf }
  | whitespaces+ { assemble_rec lexbuf }
  | symbols as c { print_token (Tsymbol c); assemble_rec lexbuf }
  | ("alf "|"ALF ") ([^'\n']* as s) { print_token (Talf s); assemble_rec lexbuf }
  | letters (letters|digits)* as s {
      let s' = String.lowercase_ascii s in
      print_token (if StringSet.mem s' ass_keywords then TassKeyword s'
                   else if StringSet.mem s' mix_keywords then TmixKeyword s'
                   else Tident s');
      assemble_rec lexbuf }
  | number as n   { print_token (Tconst (int_of_string n)); assemble_rec lexbuf }
  | _ { failwith "lexical error" }
  | eof { print_token (Teof) }
{
  let () = assemble (Lexing.from_channel (open_in filename))
}
