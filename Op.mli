type op =
  (* Chargement mémoire -> registre *)
  | LDA | LDX | LD1 | LD2 | LD3 | LD4 | LD5 | LD6
  | LDAN | LDXN | LD1N | LD2N | LD3N | LD4N | LD5N | LD6N
  (* Stockage registre -> mémoire *)
  | STA | STX | ST1 | ST2 | ST3 | ST4 | ST5 | ST6 | STJ | STZ
  (* Opérateurs arithmétiques *)
  | ADD | SUB | MUL | DIV
  (* Opérateurs de transfert d'adresses *)
  | ENTA | ENTX | ENT1 | ENT2 | ENT3 | ENT4 | ENT5 | ENT6
  | ENNA | ENNX | ENN1 | ENN2 | ENN3 | ENN4 | ENN5 | ENN6
  | INCA | INCX | INC1 | INC2 | INC3 | INC4 | INC5 | INC6
  | DECA | DECX | DEC1 | DEC2 | DEC3 | DEC4 | DEC5 | DEC6
  (* Opérateurs de comparaison *)
  | CMPA | CMPX | CMP1 | CMP2 | CMP3 | CMP4 | CMP5 | CMP6
  (* Opérateurs de saut *)
  | JMP | JSJ | JOV | JNOV
  | JL | JE | JG | JGE | JNE | JLE
  | JAN | JAZ | JAP | JANN | JANZ | JANP
  | JXN | JXZ | JXP | JXNN | JXNZ | JXNP
  | J1N | J1Z | J1P | J1NN | J1NZ | J1NP
  | J2N | J2Z | J2P | J2NN | J2NZ | J2NP
  | J3N | J3Z | J3P | J3NN | J3NZ | J3NP
  | J4N | J4Z | J4P | J4NN | J4NZ | J4NP
  | J5N | J5Z | J5P | J5NN | J5NZ | J5NP
  | J6N | J6Z | J6P | J6NN | J6NZ | J6NP
  (* Opérateurs de décalage *)
  | SLA | SRA | SLAX | SRAX | SLC | SRC
  (* Opérateurs divers *)
  | MOVE | NOP | HLT
  (* Opérateurs d'entrée-sortie *)
  | IN | OUT | IOC
  | JRED | JBUS
  (* Opérateurs de conversion *)
  | NUM | CHAR

type fvalues = AnyLR | AnyLR02 | AnyFrom1 | Int of int

type instructions_params = {
  op_code : int;
  op_variant : fvalues;
  op_normal : int
}

val fspec_to_int : int -> int -> int

val is_fspec05_valid : int -> bool
val is_fspec02_valid : int -> bool

val op_to_codes : op -> instructions_params
val string_to_op : string -> op
val is_op : string -> bool
