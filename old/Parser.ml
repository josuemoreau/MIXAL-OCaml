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

exception SyntaxError of int

let split_no_empty_strings c s =
  List.filter (fun x -> x <> "") (String.split_on_char c s)

let string_of_char = String.make 1

let cut_in_3parts line nbline =
  let tline = String.trim line in
  let parts = Array.make 3 "" in
  let j = ref 0 in
  let tmp = ref "" in
  for i = 0 to String.length tline - 1 do
    if tline.[i] = ' ' && !tmp = "" &&
       ((!j < 2 && not (is_op parts.(0))) || !j = 0) then ()
    else if tline.[i] = ' ' then begin
      if !j >= 3 then raise (SyntaxError nbline);
      parts.(!j) <- !tmp;
      tmp := "";
      incr j
    end else
      tmp := !tmp ^ string_of_char tline.[i]
  done;
  if !tmp != "" && !j < 3 then
    parts.(!j) <- !tmp
  else
    raise (SyntaxError nbline);
  (parts.(0), parts.(1), parts.(2))

let parse1 filename =
  let ic = open_in filename in
  let nbline = ref 0 in
  let symbols = ref [] in
  try
    while true do
      let line  = input_line ic in
      begin
        match cut_in_3parts line !nbline with
        | "", "", "" -> ()
        | op, "", "" -> Format.printf "@[OP: `%s`@]@." op
        | op, addr, "" -> Format.printf "OP: `%s`, ADDR: `%s`@." op addr
        | symbol, op, addr -> Format.printf "SYMBOL: `%s`, OP: `%s`, ADDR: `%s`@." symbol op addr
      end;
      incr nbline
    done
  with e ->
    close_in ic



module StringMap = Map.Make(String)

type symbols = int StringMap.t

