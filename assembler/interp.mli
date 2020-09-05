open Format
open Word
open Machine
open Io

val pp_registers  : formatter -> machine -> unit
val pp_indicators : formatter -> machine -> unit

val exec_instr : machine -> unit
val exec       : machine -> unit
