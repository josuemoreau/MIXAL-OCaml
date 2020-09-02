open Op
open Word
open Ast
open Memory

let set_word_instr w op addr index fspec =
  set_word_part 0 w addr 2;
  set_byte w 3 index;
  set_byte w 4 fspec;
  set_byte w 5 op

let fill_empty_word_mixinstr w (i: mix_instr) =
  let op = match i.op with
    | NOP -> 0
    | ADD -> 1
    | SUB -> 2
    | MUL -> 3
    | DIV -> 4
    | NUM | CHAR | HLT -> 5
    | SLA | SRA | SLAX | SRAX | SLC | SRC -> 6
    | MOVE -> 7
    | LDA -> 8
    | LD1 -> 9
    | LD2 -> 10
    | LD3 -> 11
    | LD4 -> 12
    | LD5 -> 13
    | LD6 -> 14
    | LDX -> 15
    | LDAN -> 16
    | LD1N -> 17
    | LD2N -> 18
    | LD3N -> 19
    | LD4N -> 20
    | LD5N -> 21
    | LD6N -> 22
    | LDXN -> 23
    | STA  -> 24
    | ST1  -> 25
    | ST2  -> 26
    | ST3  -> 27
    | ST4  -> 28
    | ST5  -> 29
    | ST6  -> 30
    | STX  -> 31
    | STJ  -> 32
    | STZ  -> 33
    | JBUS -> 34
    | IOC  -> 35
    | IN   -> 36
    | OUT  -> 37
    | JRED -> 38
    | JMP | JSJ | JOV | JNOV | JL | JE | JG | JGE | JNE | JLE -> 39
    | JAN | JAZ | JAP | JANN | JANZ | JANP -> 40
    | J1N | J1Z | J1P | J1NN | J1NZ | J1NP -> 41
    | J2N | J2Z | J2P | J2NN | J2NZ | J2NP -> 42
    | J3N | J3Z | J3P | J3NN | J3NZ | J3NP -> 43
    | J4N | J4Z | J4P | J4NN | J4NZ | J4NP -> 44
    | J5N | J5Z | J5P | J5NN | J5NZ | J5NP -> 45
    | J6N | J6Z | J6P | J6NN | J6NZ | J6NP -> 46
    | JXN | JXZ | JXP | JXNN | JXNZ | JXNP -> 47
    | INCA | DECA | ENTA | ENNA -> 48
    | INC1 | DEC1 | ENT1 | ENN1 -> 49
    | INC2 | DEC2 | ENT2 | ENN2 -> 50
    | INC3 | DEC3 | ENT3 | ENN3 -> 51
    | INC4 | DEC4 | ENT4 | ENN4 -> 52
    | INC5 | DEC5 | ENT5 | ENN5 -> 53
    | INC6 | DEC6 | ENT6 | ENN6 -> 54
    | INCX | DECX | ENTX | ENNX -> 55
    | CMPA -> 56
    | CMP1 -> 57
    | CMP2 -> 58
    | CMP3 -> 59
    | CMP4 -> 60
    | CMP5 -> 61
    | CMP6 -> 62
    | CMPX -> 63 in
  let addr = match i.addr with
    | AExpr (EPos (ENum i)) -> i
    | _ -> assert (false) in
  let index = match i.index with
    | IExpr (EPos (ENum i)) -> i
    | _ -> assert (false) in
  let fspec = match i.fspec with
    | FExpr (EPos (ENum i)) -> i
    | FEmpty ->
      begin match i.op with
        | JMP  | NUM  | NOP  | SLA       | JBUS | IOC | IN | OUT | JRED -> 0
        | JSJ  | CHAR | MOVE | SRA  -> 1
        | JOV  | HLT  | STJ  | SLAX -> 2
        | JNOV               | SRAX -> 3
        | JL                 | SLC  -> 4
        | JE                 | SRC  -> 5
        | JG -> 6
        | JGE -> 7
        | JNE -> 8
        | JLE -> 9
        | JAN  | J1N  | J2N  | J3N  | J4N  | J5N  | J6N  | JXN  -> 0
        | JAZ  | J1Z  | J2Z  | J3Z  | J4Z  | J5Z  | J6Z  | JXZ  -> 1
        | JAP  | J1P  | J2P  | J3P  | J4P  | J5P  | J6P  | JXP  -> 2
        | JANN | J1NN | J2NN | J3NN | J4NN | J5NN | J6NN | JXNN -> 3
        | JANZ | J1NZ | J2NZ | J3NZ | J4NZ | J5NZ | J6NZ | JXNZ -> 4
        | JANP | J1NP | J2NP | J3NP | J4NP | J5NP | J6NP | JXNP -> 5
        | INCA | INC1 | INC2 | INC3 | INC4 | INC5 | INC6 | INCX -> 0
        | DECA | DEC1 | DEC2 | DEC3 | DEC4 | DEC5 | DEC6 | DECX -> 1
        | ENTA | ENT1 | ENT2 | ENT3 | ENT4 | ENT5 | ENT6 | ENTX -> 2
        | ENNA | ENN1 | ENN2 | ENN3 | ENN4 | ENN5 | ENN6 | ENNX -> 3
        | _ -> 5
      end
    | _ -> assert (false) in
  set_word_instr w op addr index fspec

let fill_empty_word_assinstr w (i: ass_instr) =
  let v = match i.addr with
    | WAtomic (WExpr (EPos (ENum i), FEmpty)) -> i
    | _ -> assert (false) in
  match i.op with
  | CON -> set_word_part 0 w v 5
  | _ -> assert (false)

(* c is a lowercase letter, digit or symbol *)
let char_to_int c =
  match c with
  | ' '  -> 0
  | '.'  -> 40
  | ','  -> 41
  | '('  -> 42
  | ')'  -> 43
  | '+'  -> 44
  | '-'  -> 45
  | '*'  -> 46
  | '/'  -> 47
  | '='  -> 48
  | '$'  -> 49
  | '<'  -> 50
  | '>'  -> 51
  | '@'  -> 52
  | ';'  -> 53
  | ':'  -> 54
  | '\'' -> 55
  | _    ->
    let n = int_of_char c in
    if int_of_char 'a' <= n && n <= int_of_char 'i' then
      1 + (n - int_of_char 'a')
    else if int_of_char 'j' <= n && n <= int_of_char 'r' then
      11 + (n - int_of_char 'j')
    else if int_of_char 's' <= n && n <= int_of_char 'z' then
      22 + (n - int_of_char 's')
    else if int_of_char '0' <= n && n <= int_of_char '9' then
      30 + (n - int_of_char '0')
    else
      failwith "Caractère inconnu dans la définition d'une constante alphanumérique."

let fill_empty_word_alfinstr w (i: alf_instr) =
  let s = String.lowercase_ascii i.value in
  if String.length s <> 5 then
    failwith "La chaine de caractère fournie en argument à la commande ALF ne fait pas 5 caractères."
  else
    for j = 1 to 5 do
      let c = s.[j - 1] in
      set_byte w j (char_to_int c)
    done

let fill_empty_word_instr w (i: instr) =
  match i with
  | MixInstr i -> fill_empty_word_mixinstr w i
  | AssInstr i -> fill_empty_word_assinstr w i
  | AlfInstr i -> fill_empty_word_alfinstr w i

let inlined_ast_to_memory (m: memory) (t: ast) =
  let start_loc = ref 0 in
  let _ = List.fold_left (fun loc_counter i ->
      match i with
      | Instr (AssInstr i) when i.op = ORIG ->
        begin match i.addr with
          | WAtomic (WExpr (EPos (ENum i), FEmpty)) -> i
          | _ -> assert (false)
        end
      | Instr (AssInstr i) when i.op = END ->
        start_loc :=
          begin match i.addr with
            | WAtomic (WExpr (EPos (ENum i), FEmpty)) -> i
            | _ -> assert (false)
          end;
        0
      (* tous les EQU ont été enlevés, les seules autres possibilités sont END,
         qui est forcéménent à la fin, d'après un des algorithmes précédents ;
         ou CON, qui est géré par fill_empty_word_instr *)
      | Instr i ->
        fill_empty_word_instr m.(loc_counter) i;
        loc_counter + 1
      | _ -> assert (false)
    ) 0 t in
  !start_loc
