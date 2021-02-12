open Format
open Machine
open Word

val ioc     : machine -> int -> int -> unit
val input   : machine -> int -> int -> unit
val out     : machine -> int -> int -> unit
val is_busy : int -> bool
