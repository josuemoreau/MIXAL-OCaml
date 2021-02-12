open Format
open Word
open Memory

type machine

val init : int -> memory -> machine

val mem : machine -> int -> word
val get_loc_pointer : machine -> int
val set_loc_pointer : machine -> int -> unit
val next_word : machine -> word

val get_int_rA : machine -> int
val get_int_rI : machine -> int -> int
val get_int_rX : machine -> int

val to_file : machine -> string
val of_file : string -> machine
val write_file : machine -> string -> unit
val read_file : string -> machine

val add : machine -> int -> int -> unit
val sub : machine -> int -> int -> unit
val mul : machine -> int -> int -> unit
val div : machine -> int -> int -> unit

val num : machine -> unit
val char : machine -> unit

val ld_rA : machine -> int -> int -> unit
val ld_rI : machine -> int -> int -> int -> unit
val ld_rX : machine -> int -> int -> unit

val ldn_rA : machine -> int -> int -> unit
val ldn_rI : machine -> int -> int -> int -> unit
val ldn_rX : machine -> int -> int -> unit

val st_rA : machine -> int -> int -> unit
val st_rI : machine -> int -> int -> int -> unit
val st_rX : machine -> int -> int -> unit
val st_rJ : machine -> int -> int -> unit
val st_z : machine -> int -> int -> unit

val ent_rA : machine -> bool -> int -> unit
val ent_rI : machine -> int -> bool -> int -> unit
val ent_rX : machine -> bool -> int -> unit

val enn_rA : machine -> bool -> int -> unit
val enn_rI : machine -> int -> bool -> int -> unit
val enn_rX : machine -> bool -> int -> unit

val inc_rA : machine -> int -> unit
val inc_rI : machine -> int -> int -> unit
val inc_rX : machine -> int -> unit

val dec_rA : machine -> int -> unit
val dec_rI : machine -> int -> int -> unit
val dec_rX : machine -> int -> unit

val move : machine -> int -> int -> unit

val is_z : word -> bool
val is_p : word -> bool
val is_n : word -> bool
val is_nz : word -> bool
val is_np : word -> bool
val is_nn : word -> bool

val is2_z : word -> bool
val is2_p : word -> bool
val is2_n : word -> bool
val is2_nz : word -> bool
val is2_np : word -> bool
val is2_nn : word -> bool

val jmp : machine -> int -> int
val jsj : machine -> int -> int
val j_rA : machine -> int -> (word -> bool) -> int
val j_rI : machine -> int -> int -> (word -> bool) -> int
val j_rX : machine -> int -> (word -> bool) -> int

val jov : machine -> int -> int
val jnov : machine -> int -> int
val jl : machine -> int -> int
val je : machine -> int -> int
val jg : machine -> int -> int
val jge : machine -> int -> int
val jne : machine -> int -> int
val jle : machine -> int -> int

val cmp_rA : machine -> int -> int -> unit
val cmp_rI : machine -> int -> int -> int -> unit
val cmp_rX : machine -> int -> int -> unit

val sla : machine -> int -> unit
val sra : machine -> int -> unit
val slax : machine -> int -> unit
val srax : machine -> int -> unit
val slc : machine -> int -> unit
val src : machine -> int -> unit

val pp_registers  : formatter -> machine -> unit
val pp_indicators : formatter -> machine -> unit
val pp_memory : formatter -> machine -> unit
