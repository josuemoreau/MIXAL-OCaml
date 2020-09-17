open Format
open Word

type memory = word array

val empty : unit -> memory
val pp_memory : formatter -> memory -> unit

val to_file : memory -> string
val of_file : string -> memory
