open Format
open Word

type memory = word array

val empty : unit -> memory
val pp_memory : formatter -> memory -> unit
