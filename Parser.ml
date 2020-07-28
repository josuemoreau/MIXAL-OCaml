open Word
open Op
open Instruction

type parsing_line = {
  line    : int;
  label   : string option;
  op      : op;
  address : string;
  index   : string;
  fspec   : string
}

type symbol_string  = string * string
type symbols_string = symbol_string list

let split_no_empty_strings c s =
  List.filter (fun x -> x <> "") (String.split_on_char c s)

let parse1 filename =
  let ic = open_in filename in
  let symbols = ref [] in
  try
    let line  = input_line ic in
    let tmp = ref "" in
    for i = 0 to String.length line - 1 do
      if line.[i] = ' ' then
        
    done
  with e ->
    close_in ic






module StringMap = Map.Make(String)

type symbols = int StringMap.t
