open Format

type byte

type word

exception Overflow

val empty : unit -> word
val byte_size : int
val word_size : int

val set_sign : word -> bool -> unit
val get_sign : word -> bool

val set_byte : word -> int -> int -> unit
val get_byte : word -> int -> int

val pp_word : formatter -> word -> unit

val set_zero : word -> int -> unit
val set_word_part : int -> word -> int -> int -> unit
val set_word_part2 : int -> word -> int -> unit

val set_sub_f : word -> word -> int -> unit
val set_sub2_f : word -> word -> int -> unit

val set_sub_shift_f : word -> word -> int -> unit
val set_sub_shift_bis_f : word -> word -> int -> unit
val set_sub_shift_bis2_f : word -> word -> int -> unit

val to_int : word -> int
val to_int2 : word -> int
val get_word_part : word -> int -> int
val get_word_part2 : word -> int -> int

val is_null : word -> bool

val to_file_format : word -> string
val of_file_format : word -> string -> unit
