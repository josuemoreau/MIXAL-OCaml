open Word
open Op

type instr = {
  op      : op;
  address : int;
  index   : int;
  fspec   : int
}

val to_word : op -> int -> int option -> int option -> int -> word
val string_to_op : string -> op
