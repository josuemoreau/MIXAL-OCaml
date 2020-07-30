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
  op_code : int;        (* code de l'opérateur *)
  op_variant : fvalues; (* code de la variante de l'opérateur si plusieurs
                           opérateurs sont définis sur le même op_code *)
  op_normal : int       (* valeur par défaut de la F-specification *)
}

let params c f n = { op_code = c; op_variant = f; op_normal = n }

let fspec_to_int l r = 8 * l + r

let is_fspec05_valid f =
  0 <= f / 8 && f / 8 <= 5 && 0 <= f mod 8 && f mod 8 <= 5

let is_fspec02_valid f =
  0 <= f / 8 && f / 8 <= 2 && 0 <= f mod 8 && f mod 8 <= 2

let op_to_codes i =
  let lr05 = fspec_to_int 0 5 in
  let lr02 = fspec_to_int 0 2 in
  match i with
  | NOP  -> params  0 AnyLR 0
  | ADD  -> params  1 AnyLR lr05
  | SUB  -> params  2 AnyLR lr05
  | MUL  -> params  3 AnyLR lr05
  | DIV  -> params  4 AnyLR lr05
  | NUM  -> params  5 (Int 0) 0
  | CHAR -> params  5 (Int 1) 1
  | HLT  -> params  5 (Int 2) 2
  | SLA  -> params  6 (Int 0) 0
  | SRA  -> params  6 (Int 1) 1
  | SLAX -> params  6 (Int 2) 2
  | SRAX -> params  6 (Int 3) 3
  | SLC  -> params  6 (Int 4) 4
  | SRC  -> params  6 (Int 5) 5
  | MOVE -> params  7 AnyFrom1 1
  | LDA  -> params  8 AnyLR lr05
  | LD1  -> params  9 AnyLR lr05
  | LD2  -> params 10 AnyLR lr05
  | LD3  -> params 11 AnyLR lr05
  | LD4  -> params 12 AnyLR lr05
  | LD5  -> params 13 AnyLR lr05
  | LD6  -> params 14 AnyLR lr05
  | LDX  -> params 15 AnyLR lr05
  | LDAN -> params 16 AnyLR lr05
  | LD1N -> params 17 AnyLR lr05
  | LD2N -> params 18 AnyLR lr05
  | LD3N -> params 19 AnyLR lr05
  | LD4N -> params 20 AnyLR lr05
  | LD5N -> params 21 AnyLR lr05
  | LD6N -> params 22 AnyLR lr05
  | LDXN -> params 23 AnyLR lr05
  | STA  -> params 24 AnyLR lr05
  | ST1  -> params 25 AnyLR lr05
  | ST2  -> params 26 AnyLR lr05
  | ST3  -> params 27 AnyLR lr05
  | ST4  -> params 28 AnyLR lr05
  | ST5  -> params 29 AnyLR lr05
  | ST6  -> params 30 AnyLR lr05
  | STX  -> params 31 AnyLR lr05
  | STJ  -> params 32 AnyLR02 lr02
  | STZ  -> params 33 AnyLR lr05
  | JBUS -> params 34 (Int 0) 0
  | IOC  -> params 35 (Int 0) 0
  | IN   -> params 36 (Int 0) 0
  | OUT  -> params 37 (Int 0) 0
  | JRED -> params 38 (Int 0) 0
  | JMP  -> params 39 (Int 0) 0
  | JSJ  -> params 39 (Int 1) 1
  | JOV  -> params 39 (Int 2) 2
  | JNOV -> params 39 (Int 3) 3
  | JL   -> params 39 (Int 4) 4
  | JE   -> params 39 (Int 5) 5
  | JG   -> params 39 (Int 6) 6
  | JGE  -> params 39 (Int 7) 7
  | JNE  -> params 39 (Int 8) 8
  | JLE  -> params 39 (Int 9) 9
  | JAN  -> params 40 (Int 0) 0
  | JAZ  -> params 40 (Int 1) 1
  | JAP  -> params 40 (Int 2) 2
  | JANN -> params 40 (Int 3) 3
  | JANZ -> params 40 (Int 4) 4
  | JANP -> params 40 (Int 5) 5
  | J1N  -> params 41 (Int 0) 0
  | J1Z  -> params 41 (Int 1) 1
  | J1P  -> params 41 (Int 2) 2
  | J1NN -> params 41 (Int 3) 3
  | J1NZ -> params 41 (Int 4) 4
  | J1NP -> params 41 (Int 5) 5
  | J2N  -> params 42 (Int 0) 0
  | J2Z  -> params 42 (Int 1) 1
  | J2P  -> params 42 (Int 2) 2
  | J2NN -> params 42 (Int 3) 3
  | J2NZ -> params 42 (Int 4) 4
  | J2NP -> params 42 (Int 5) 5
  | J3N  -> params 43 (Int 0) 0
  | J3Z  -> params 43 (Int 1) 1
  | J3P  -> params 43 (Int 2) 2
  | J3NN -> params 43 (Int 3) 3
  | J3NZ -> params 43 (Int 4) 4
  | J3NP -> params 43 (Int 5) 5
  | J4N  -> params 44 (Int 0) 0
  | J4Z  -> params 44 (Int 1) 1
  | J4P  -> params 44 (Int 2) 2
  | J4NN -> params 44 (Int 3) 3
  | J4NZ -> params 44 (Int 4) 4
  | J4NP -> params 44 (Int 5) 5
  | J5N  -> params 45 (Int 0) 0
  | J5Z  -> params 45 (Int 1) 1
  | J5P  -> params 45 (Int 2) 2
  | J5NN -> params 45 (Int 3) 3
  | J5NZ -> params 45 (Int 4) 4
  | J5NP -> params 45 (Int 5) 5
  | J6N  -> params 46 (Int 0) 0
  | J6Z  -> params 46 (Int 1) 1
  | J6P  -> params 46 (Int 2) 2
  | J6NN -> params 46 (Int 3) 3
  | J6NZ -> params 46 (Int 4) 4
  | J6NP -> params 46 (Int 5) 5
  | JXN  -> params 47 (Int 0) 0
  | JXZ  -> params 47 (Int 1) 1
  | JXP  -> params 47 (Int 2) 2
  | JXNN -> params 47 (Int 3) 3
  | JXNZ -> params 47 (Int 4) 4
  | JXNP -> params 47 (Int 5) 5
  | INCA -> params 48 (Int 0) 0
  | DECA -> params 48 (Int 1) 1
  | ENTA -> params 48 (Int 2) 2
  | ENNA -> params 48 (Int 3) 3
  | INC1 -> params 49 (Int 0) 0
  | DEC1 -> params 49 (Int 1) 1
  | ENT1 -> params 49 (Int 2) 2
  | ENN1 -> params 49 (Int 3) 3
  | INC2 -> params 50 (Int 0) 0
  | DEC2 -> params 50 (Int 1) 1
  | ENT2 -> params 50 (Int 2) 2
  | ENN2 -> params 50 (Int 3) 3
  | INC3 -> params 51 (Int 0) 0
  | DEC3 -> params 51 (Int 1) 1
  | ENT3 -> params 51 (Int 2) 2
  | ENN3 -> params 51 (Int 3) 3
  | INC4 -> params 52 (Int 0) 0
  | DEC4 -> params 52 (Int 1) 1
  | ENT4 -> params 52 (Int 2) 2
  | ENN4 -> params 52 (Int 3) 3
  | INC5 -> params 53 (Int 0) 0
  | DEC5 -> params 53 (Int 1) 1
  | ENT5 -> params 53 (Int 2) 2
  | ENN5 -> params 53 (Int 3) 3
  | INC6 -> params 54 (Int 0) 0
  | DEC6 -> params 54 (Int 1) 1
  | ENT6 -> params 54 (Int 2) 2
  | ENN6 -> params 54 (Int 3) 3
  | INCX -> params 55 (Int 0) 0
  | DECX -> params 55 (Int 1) 1
  | ENTX -> params 55 (Int 2) 2
  | ENNX -> params 55 (Int 3) 3
  | CMPA -> params 56 AnyLR lr05
  | CMP1 -> params 57 AnyLR lr05
  | CMP2 -> params 58 AnyLR lr05
  | CMP3 -> params 59 AnyLR lr05
  | CMP4 -> params 60 AnyLR lr05
  | CMP5 -> params 61 AnyLR lr05
  | CMP6 -> params 62 AnyLR lr05
  | CMPX -> params 63 AnyLR lr05

exception UnknownOperator of string

let string_to_op s =
  match String.uppercase_ascii s with
  | "LDA" -> LDA | "LDX" -> LDX
  | "LD1" -> LD1 | "LD2" -> LD2 | "LD3" -> LD3
  | "LD4" -> LD4 | "LD5" -> LD5 | "LD6" -> LD6
  | "LDAN" -> LDAN | "LDXN" -> LDXN
  | "LD1N" -> LD1N | "LD2N" -> LD2N | "LD3N" -> LD3N
  | "LD4N" -> LD4N | "LD5N" -> LD5N | "LD6N" -> LD6N
  (* Stockage registre -> mémoire *)
  | "STA" -> STA | "STX" -> STX
  | "ST1" -> ST1 | "ST2" -> ST2 | "ST3" -> ST3
  | "ST4" -> ST4 | "ST5" -> ST5 | "ST6" -> ST6
  | "STJ" -> STJ | "STZ" -> STZ
  (* Opérateurs arithmétiques *)
  | "ADD" -> ADD | "SUB" -> SUB | "MUL" -> MUL | "DIV" -> DIV
  (* Opérateurs de transfert d'adresses *)
  | "ENTA" -> ENTA | "ENTX" -> ENTX
  | "ENT1" -> ENT1 | "ENT2" -> ENT2 | "ENT3" -> ENT3
  | "ENT4" -> ENT4 | "ENT5" -> ENT5 | "ENT6" -> ENT6
  | "ENNA" -> ENNA | "ENNX" -> ENNX
  | "ENN1" -> ENN1 | "ENN2" -> ENN2 | "ENN3" -> ENN3
  | "ENN4" -> ENN4 | "ENN5" -> ENN5 | "ENN6" -> ENN6
  | "INCA" -> INCA | "INCX" -> INCX
  | "INC1" -> INC1 | "INC2" -> INC2 | "INC3" -> INC3
  | "INC4" -> INC4 | "INC5" -> INC5 | "INC6" -> INC6
  | "DECA" -> DECA | "DECX" -> DECX
  | "DEC1" -> DEC1 | "DEC2" -> DEC2 | "DEC3" -> DEC3
  | "DEC4" -> DEC4 | "DEC5" -> DEC5 | "DEC6" -> DEC6
  (* Opérateurs de comparaison *)
  | "CMPA" -> CMPA | "CMPX" -> CMPX
  | "CMP1" -> CMP1 | "CMP2" -> CMP2 | "CMP3" -> CMP3
  | "CMP4" -> CMP4 | "CMP5" -> CMP5 | "CMP6" -> CMP6
  (* Opérateurs de saut *)
  | "JMP" -> JMP | "JSJ" -> JSJ | "JOV" -> JOV | "JNOV" -> JNOV
  | "JL" -> JL | "JE" -> JE | "JG" -> JG
  | "JGE" -> JGE | "JNE" -> JNE | "JLE" -> JLE
  | "JAN" -> JAN | "JAZ" -> JAZ | "JAP" -> JAP
  | "JANN" -> JANN | "JANZ" -> JANZ | "JANP" -> JANP
  | "JXN" -> JXN | "JXZ" -> JXZ | "JXP" -> JXP
  | "JXNN" -> JXNN | "JXNZ" -> JXNZ | "JXNP" -> JXNP
  | "J1N" -> J1N | "J1Z" -> J1Z | "J1P" -> J1P
  | "J1NN" -> J1NN | "J1NZ" -> J1NZ | "J1NP" -> J1NP
  | "J2N" -> J2N | "J2Z" -> J2Z | "J2P" -> J2P
  | "J2NN" -> J2NN | "J2NZ" -> J2NZ | "J2NP" -> J2NP
  | "J3N" -> J3N | "J3Z" -> J3Z | "J3P" -> J3P
  | "J3NN" -> J3NN | "J3NZ" -> J3NZ | "J3NP" -> J3NP
  | "J4N" -> J4N | "J4Z" -> J4Z | "J4P" -> J4P
  | "J4NN" -> J4NN | "J4NZ" -> J4NZ | "J4NP" -> J4NP
  | "J5N" -> J5N | "J5Z" -> J5Z | "J5P" -> J5P
  | "J5NN" -> J5NN | "J5NZ" -> J5NZ | "J5NP" -> J5NP
  | "J6N" -> J6N | "J6Z" -> J6Z | "J6P" -> J6P
  | "J6NN" -> J6NN | "J6NZ" -> J6NZ | "J6NP" -> J6NP
  (* Opérateurs de décalage *)
  | "SLA" -> SLA | "SRA" -> SRA
  | "SLAX" -> SLAX | "SRAX" -> SRAX
  | "SLC" -> SLC | "SRC" -> SRC
  (* Opérateurs divers *)
  | "MOVE" -> MOVE | "NOP" -> NOP | "HLT" -> HLT
  (* Opérateurs d'entrée-sortie *)
  | "IN" -> IN | "OUT" -> OUT | "IOC" -> IOC
  | "JRED" -> JRED | "JBUS" -> JBUS
  (* Opérateurs de conversion *)
  | "NUM" -> NUM | "CHAR" -> CHAR
  | _ -> raise (UnknownOperator s)

let is_op s =
  try ignore (string_to_op s); true with UnknownOperator s -> false
