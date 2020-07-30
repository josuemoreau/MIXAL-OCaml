open Word
open Op
open Instruction
open Parser

let () =
  let w = to_word ENT3 (-5) (Some 1) None 1 in
  print w

let () = parse1 Sys.argv.(1)

