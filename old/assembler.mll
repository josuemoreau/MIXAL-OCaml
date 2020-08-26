{
  open Format

  module StringSet = Set.Make(String)
  module StringMap = Map.Make(String)

  type tokens =
    | Tident of string
    | TassKeyword of string
    | TmixKeyword of string
    | Tsymbol of string
    | Tconst of int
    | Talf of string
    | Teof

  type instrpos = INSTR | ADDR | INDEX | FSPEC | END

  type state = {
      mutable instrpos   : instrpos;
      mutable lastnb     : int option;
      mutable lastbinop  : string option;
      mutable bindings   : int StringMap.t;
      mutable loccounter : int;
      mutable lastassinstr  : string option;
      mutable lastsymdef : string option
  }

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
  let special_symbols = StringSet.of_list [
      "1H"; "1B"; "1F"; "2H"; "2B"; "2F"; "3H"; "3B"; "3F"; "4H"; "4B"; "4F";
      "5H"; "5B"; "5F"; "6H"; "6B"; "6F"; "7H"; "7B"; "7F"; "8H"; "8B"; "8F";
      "9H"; "9B"; "9F"; "0H"; "0B"; "0F"
  ]

  let filename = Sys.argv.(1)

  let pp f = function
    | Tident s      -> printf "IDENT(%s)" s
    | TassKeyword s -> printf "ASS(%s)" s
    | TmixKeyword s -> printf "MIX(%s)" s
    | Tsymbol s -> printf "SYM(%s)" s
    | Tconst n -> printf "CONST(%d)" n
    | Talf s   -> printf "ALF(%s)" s
    | Teof     -> ()

  let print_token = printf "%a " pp

  let rec base f b n =
    if n < b then f n
    else begin
      f (n / b);
      base f b (n mod b)
    end

  let change_instrpos state pos =
    begin match state.lastnb with
    | None -> ()
    | Some nb -> printf "--> %d" nb
    end;
    state.instrpos  <- pos;
    state.lastnb    <- None;
    state.lastbinop <- None

  let reset_instr state =
    begin match state.lastassinstr, state.lastsymdef with
      | Some "equ", Some sym ->
        begin match state.lastnb with
          | None -> failwith "Valeur de l'équivalence non définie."
          | Some nb -> state.bindings <- StringMap.add sym nb state.bindings
        end
      | Some "equ", None ->
          failwith "Impossible de définir une équivalence à un symbole sans nom."
      | None, None
      | Some _, None -> ()
      | None, Some sym
      | Some _, Some sym ->
          state.bindings <- StringMap.add sym state.loccounter state.bindings
    end;
    change_instrpos state INSTR;
    state.lastassinstr <- None;
    state.lastsymdef <- None

  let compute op nb1 nb2 =
    match op with
    | "+" -> nb1 + nb2
    | "-" -> nb1 - nb2
    | "*" -> nb1 * nb2
    | "/" -> nb1 / nb2
    | "//" -> (nb1 * Word.word_max) / nb2
    | ":" -> ((nb1 * 8) mod Word.word_max) + nb2
    | _ -> failwith (op ^ " n'est pas un opérateur binaire.")

  let computation_step state nb =
    match state.lastbinop, state.lastnb with
    (* constante sans opérateurs *)
    | None, _ -> state.lastnb <- Some nb
    (* opérateurs unaires *)
    | Some "+", None ->
      state.lastnb <- Some(nb);
      state.lastbinop <- None
    | Some "-", None ->
      state.lastnb <- Some (- nb);
      state.lastbinop <- None;
    | Some op, None -> failwith (op ^ " n'est pas un opérateur unaire.")
    (* opérateurs binaires *)
    | Some op, Some nb' ->
      state.lastnb <- Some (compute op nb' nb);
      state.lastbinop <- None

}

let letters = ['a'-'z' 'A'-'Z']
let digits  = ['0'-'9']
let number  = digits+
let whitespaces = [' ' '\t' '\n']
let symbols = "+"|"-"|"*"|"/"|"//"|"="|","|"("|")"|":"

rule assemble state = parse
  | whitespaces* '*' [^'\n']* { assemble_rec state Word.empty lexbuf }
  | _ { assemble_rec state Word.empty lexbuf }
and assemble_rec state word = parse
  | '\n' whitespaces* '*' [^'\n']* '\n' {
      reset_instr state;
      assemble_rec state Word.empty lexbuf
    }
  | whitespaces* '\n' whitespaces* {
      reset_instr state;
      printf "\n";
      assemble_rec state Word.empty lexbuf
    }
  | whitespaces+ {
      (* tous les espaces blancs ne contenant pas de retour à la ligne *)
      assemble_rec state word lexbuf
    }
  | symbols as s {
      print_token (Tsymbol s);
      match s with
      | "+" | "-" | "*" | "/" | "//" | ":" ->
        state.lastbinop <- Some s; assemble_rec state word lexbuf
      | "," ->
        change_instrpos state INDEX; assemble_rec state word lexbuf
      | "(" ->
        change_instrpos state FSPEC; assemble_rec state word lexbuf
      | ")" ->
        change_instrpos state END; assemble_rec state word lexbuf
      | "=" ->
        (* cas à traiter *)
        assemble_rec state word lexbuf
      | _ -> assemble_rec state word lexbuf
    }
  | ("alf "|"ALF ") ([^'\n']* as s) {
      print_token (Talf s);
      (* à traiter, pour l'écriture d'une constante alphanumérique *)
      assemble_rec state Word.empty lexbuf
    }
  | number as n   {
      let nb = int_of_string n in
      print_token (Tconst nb);
      computation_step state nb;
      assemble_rec state word lexbuf
    }
  | (letters|digits)+ as s {
      let s' = String.lowercase_ascii s in
      print_token (if StringSet.mem s' ass_keywords then TassKeyword s'
                   else if StringSet.mem s' mix_keywords then TmixKeyword s'
                   else Tident s');
      begin match state.instrpos with
      | INSTR when StringSet.mem s' ass_keywords ->
            if state.lastassinstr = None then state.lastassinstr <- Some s'
            else failwith (s' ^ " est un mot clé, il ne peut pas être un symbole.");
            change_instrpos state ADDR
      | INSTR when StringSet.mem s' mix_keywords ->
            (* remplissage du word avec les codes spécifiques à l'instruction *)
            change_instrpos state ADDR
      | INSTR ->
        printf " (DEF SYMBOLE %s) " s';
        if state.lastsymdef = None then state.lastsymdef <- Some s'
                 else failwith (s' ^ " n'est pas une instruction.")
      | ADDR | INDEX | FSPEC ->
        begin try
          computation_step state (StringMap.find s' state.bindings)
        with Not_found -> failwith (s' ^ " n'est pas un symbole défini.") end
      | END   -> ()
      (* à traiter, pour l'affectation des zones du mot en fonction de
         l'instruction *)
      end;
      assemble_rec state Word.empty lexbuf
    }
  | _ { failwith "lexical error" }
  | eof { print_token (Teof) }
{
  let state = {
      instrpos   = ADDR;
      lastnb     = None;
      lastbinop  = None;
      bindings   = StringMap.empty;
      loccounter = 0;
      lastassinstr  = None;
      lastsymdef = None
    }
  let () = assemble state (Lexing.from_channel (open_in filename))
}
