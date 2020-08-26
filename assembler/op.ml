type mixop =
  | NOP
  | ADD | SUB | MUL | DIV
  | NUM | CHAR | HLT
  | SLA | SRA | SLAX | SRAX | SLC | SRC
  | MOVE
  | LDA | LD1 | LD2 | LD3 | LD4 | LD5 | LD6 | LDX
  | LDAN | LD1N | LD2N | LD3N | LD4N | LD5N | LD6N | LDXN
  | STA | ST1 | ST2 | ST3 | ST4 | ST5 | ST6 | STX | STJ | STZ
  | JBUS | IOC | IN | OUT | JRED
  | JMP | JSJ | JOV | JNOV | JL | JE | JG | JGE | JNE | JLE
  | JAN | JAZ | JAP | JANN | JANZ | JANP
  | J1N | J1Z | J1P | J1NN | J1NZ | J1NP
  | J2N | J2Z | J2P | J2NN | J2NZ | J2NP
  | J3N | J3Z | J3P | J3NN | J3NZ | J3NP
  | J4N | J4Z | J4P | J4NN | J4NZ | J4NP
  | J5N | J5Z | J5P | J5NN | J5NZ | J5NP
  | J6N | J6Z | J6P | J6NN | J6NZ | J6NP
  | JXN | JXZ | JXP | JXNN | JXNZ | JXNP
  | INCA | DECA | ENTA | ENNA
  | INC1 | DEC1 | ENT1 | ENN1
  | INC2 | DEC2 | ENT2 | ENN2
  | INC3 | DEC3 | ENT3 | ENN3
  | INC4 | DEC4 | ENT4 | ENN4
  | INC5 | DEC5 | ENT5 | ENN5
  | INC6 | DEC6 | ENT6 | ENN6
  | INCX | DECX | ENTX | ENNX
  | CMPA | CMP1 | CMP2 | CMP3 | CMP4 | CMP5 | CMP6 | CMPX

type assop =
  | EQU | ORIG | CON | END

let str_of_mixop = function
| NOP -> "nop"
| ADD -> "add"
| SUB -> "sub"
| MUL -> "mul"
| DIV -> "div"
| NUM -> "num"
| CHAR -> "char"
| HLT -> "hlt"
| SLA -> "sla"
| SRA -> "sra"
| SLAX -> "slax"
| SRAX -> "srax"
| SLC -> "slc"
| SRC -> "src"
| MOVE -> "move"
| LDA -> "lda"
| LD1 -> "ld1"
| LD2 -> "ld2"
| LD3 -> "ld3"
| LD4 -> "ld4"
| LD5 -> "ld5"
| LD6 -> "ld6"
| LDX -> "ldx"
| LDAN -> "ldan"
| LD1N -> "ld1n"
| LD2N -> "ld2n"
| LD3N -> "ld3n"
| LD4N -> "ld4n"
| LD5N -> "ld5n"
| LD6N -> "ld6n"
| LDXN -> "ldxn"
| STA -> "sta"
| ST1 -> "st1"
| ST2 -> "st2"
| ST3 -> "st3"
| ST4 -> "st4"
| ST5 -> "st5"
| ST6 -> "st6"
| STX -> "stx"
| STJ -> "stj"
| STZ -> "stz"
| JBUS -> "jbus"
| IOC -> "ioc"
| IN -> "in"
| OUT -> "out"
| JRED -> "jred"
| JMP -> "jmp"
| JSJ -> "jsj"
| JOV -> "jov"
| JNOV -> "jnov"
| JL -> "jl"
| JE -> "je"
| JG -> "jg"
| JGE -> "jge"
| JNE -> "jne"
| JLE -> "jle"
| JAN -> "jan"
| JAZ -> "jaz"
| JAP -> "jap"
| JANN -> "jann"
| JANZ -> "janz"
| JANP -> "janp"
| J1N -> "j1n"
| J1Z -> "j1z"
| J1P -> "j1p"
| J1NN -> "j1nn"
| J1NZ -> "j1nz"
| J1NP -> "j1np"
| J2N -> "j2n"
| J2Z -> "j2z"
| J2P -> "j2p"
| J2NN -> "j2nn"
| J2NZ -> "j2nz"
| J2NP -> "j2np"
| J3N -> "j3n"
| J3Z -> "j3z"
| J3P -> "j3p"
| J3NN -> "j3nn"
| J3NZ -> "j3nz"
| J3NP -> "j3np"
| J4N -> "j4n"
| J4Z -> "j4z"
| J4P -> "j4p"
| J4NN -> "j4nn"
| J4NZ -> "j4nz"
| J4NP -> "j4np"
| J5N -> "j5n"
| J5Z -> "j5z"
| J5P -> "j5p"
| J5NN -> "j5nn"
| J5NZ -> "j5nz"
| J5NP -> "j5np"
| J6N -> "j6n"
| J6Z -> "j6z"
| J6P -> "j6p"
| J6NN -> "j6nn"
| J6NZ -> "j6nz"
| J6NP -> "j6np"
| JXN -> "jxn"
| JXZ -> "jxz"
| JXP -> "jxp"
| JXNN -> "jxnn"
| JXNZ -> "jxnz"
| JXNP -> "jxnp"
| INCA -> "inca"
| DECA -> "deca"
| ENTA -> "enta"
| ENNA -> "enna"
| INC1 -> "inc1"
| DEC1 -> "dec1"
| ENT1 -> "ent1"
| ENN1 -> "enn1"
| INC2 -> "inc2"
| DEC2 -> "dec2"
| ENT2 -> "ent2"
| ENN2 -> "enn2"
| INC3 -> "inc3"
| DEC3 -> "dec3"
| ENT3 -> "ent3"
| ENN3 -> "enn3"
| INC4 -> "inc4"
| DEC4 -> "dec4"
| ENT4 -> "ent4"
| ENN4 -> "enn4"
| INC5 -> "inc5"
| DEC5 -> "dec5"
| ENT5 -> "ent5"
| ENN5 -> "enn5"
| INC6 -> "inc6"
| DEC6 -> "dec6"
| ENT6 -> "ent6"
| ENN6 -> "enn6"
| INCX -> "incx"
| DECX -> "decx"
| ENTX -> "entx"
| ENNX -> "ennx"
| CMPA -> "cmpa"
| CMP1 -> "cmp1"
| CMP2 -> "cmp2"
| CMP3 -> "cmp3"
| CMP4 -> "cmp4"
| CMP5 -> "cmp5"
| CMP6 -> "cmp6"
| CMPX -> "cmpx"

let mixop_of_str = function
| "nop" -> NOP
| "add" -> ADD
| "sub" -> SUB
| "mul" -> MUL
| "div" -> DIV
| "num" -> NUM
| "char" -> CHAR
| "hlt" -> HLT
| "sla" -> SLA
| "sra" -> SRA
| "slax" -> SLAX
| "srax" -> SRAX
| "slc" -> SLC
| "src" -> SRC
| "move" -> MOVE
| "lda" -> LDA
| "ld1" -> LD1
| "ld2" -> LD2
| "ld3" -> LD3
| "ld4" -> LD4
| "ld5" -> LD5
| "ld6" -> LD6
| "ldx" -> LDX
| "ldan" -> LDAN
| "ld1n" -> LD1N
| "ld2n" -> LD2N
| "ld3n" -> LD3N
| "ld4n" -> LD4N
| "ld5n" -> LD5N
| "ld6n" -> LD6N
| "ldxn" -> LDXN
| "sta" -> STA
| "st1" -> ST1
| "st2" -> ST2
| "st3" -> ST3
| "st4" -> ST4
| "st5" -> ST5
| "st6" -> ST6
| "stx" -> STX
| "stj" -> STJ
| "stz" -> STZ
| "jbus" -> JBUS
| "ioc" -> IOC
| "in" -> IN
| "out" -> OUT
| "jred" -> JRED
| "jmp" -> JMP
| "jsj" -> JSJ
| "jov" -> JOV
| "jnov" -> JNOV
| "jl" -> JL
| "je" -> JE
| "jg" -> JG
| "jge" -> JGE
| "jne" -> JNE
| "jle" -> JLE
| "jan" -> JAN
| "jaz" -> JAZ
| "jap" -> JAP
| "jann" -> JANN
| "janz" -> JANZ
| "janp" -> JANP
| "j1n" -> J1N
| "j1z" -> J1Z
| "j1p" -> J1P
| "j1nn" -> J1NN
| "j1nz" -> J1NZ
| "j1np" -> J1NP
| "j2n" -> J2N
| "j2z" -> J2Z
| "j2p" -> J2P
| "j2nn" -> J2NN
| "j2nz" -> J2NZ
| "j2np" -> J2NP
| "j3n" -> J3N
| "j3z" -> J3Z
| "j3p" -> J3P
| "j3nn" -> J3NN
| "j3nz" -> J3NZ
| "j3np" -> J3NP
| "j4n" -> J4N
| "j4z" -> J4Z
| "j4p" -> J4P
| "j4nn" -> J4NN
| "j4nz" -> J4NZ
| "j4np" -> J4NP
| "j5n" -> J5N
| "j5z" -> J5Z
| "j5p" -> J5P
| "j5nn" -> J5NN
| "j5nz" -> J5NZ
| "j5np" -> J5NP
| "j6n" -> J6N
| "j6z" -> J6Z
| "j6p" -> J6P
| "j6nn" -> J6NN
| "j6nz" -> J6NZ
| "j6np" -> J6NP
| "jxn" -> JXN
| "jxz" -> JXZ
| "jxp" -> JXP
| "jxnn" -> JXNN
| "jxnz" -> JXNZ
| "jxnp" -> JXNP
| "inca" -> INCA
| "deca" -> DECA
| "enta" -> ENTA
| "enna" -> ENNA
| "inc1" -> INC1
| "dec1" -> DEC1
| "ent1" -> ENT1
| "enn1" -> ENN1
| "inc2" -> INC2
| "dec2" -> DEC2
| "ent2" -> ENT2
| "enn2" -> ENN2
| "inc3" -> INC3
| "dec3" -> DEC3
| "ent3" -> ENT3
| "enn3" -> ENN3
| "inc4" -> INC4
| "dec4" -> DEC4
| "ent4" -> ENT4
| "enn4" -> ENN4
| "inc5" -> INC5
| "dec5" -> DEC5
| "ent5" -> ENT5
| "enn5" -> ENN5
| "inc6" -> INC6
| "dec6" -> DEC6
| "ent6" -> ENT6
| "enn6" -> ENN6
| "incx" -> INCX
| "decx" -> DECX
| "entx" -> ENTX
| "ennx" -> ENNX
| "cmpa" -> CMPA
| "cmp1" -> CMP1
| "cmp2" -> CMP2
| "cmp3" -> CMP3
| "cmp4" -> CMP4
| "cmp5" -> CMP5
| "cmp6" -> CMP6
| "cmpx" -> CMPX
| _ -> assert (false)

let assop_of_str = function
  | "equ" -> EQU
  | "orig" -> ORIG
  | "con" -> CON
  | "end" -> END
  | _ -> assert (false)

let str_of_assop = function
  | EQU -> "equ"
  | ORIG -> "orig"
  | CON -> "con"
  | END -> "end"

