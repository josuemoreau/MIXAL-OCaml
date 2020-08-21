type byte = int

type word

val empty : word

val get_sign : word -> bool
val set_sign : word -> bool -> unit

val get_byte : word -> int -> byte
val set_byte : word -> int -> byte -> unit

val set_sub : word -> word -> int -> int -> unit
val set_sub_rightmost : word -> word -> int -> int -> unit

val print : word -> unit
