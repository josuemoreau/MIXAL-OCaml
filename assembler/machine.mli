open Format
open Word
open Memory

type machine

val init : int -> memory -> machine

val mem : machine -> int -> word
val get_loc_pointer : machine -> int
val set_loc_pointer : machine -> int -> unit
val next_word : machine -> word

val add : machine -> int -> int -> unit
val sub : machine -> int -> int -> unit
val mul : machine -> int -> int -> unit
val div : machine -> int -> int -> unit

val num : machine -> unit
val char : machine -> unit

val ld_rA : machine -> int -> int -> unit
val ld_rI : machine -> int -> int -> int -> unit
val ld_rX : machine -> int -> int -> unit

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
val int_rI : machine -> int -> int -> unit
val inc_rX : machine -> int -> unit

val dec_rA : machine -> int -> unit
val det_rI : machine -> int -> int -> unit
val dec_rX : machine -> int -> unit

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
