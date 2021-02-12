open Format
open Word

type memory = word array

let empty () =
  let a = Array.make 4000 (Word.empty ()) in
  for i = 1 to 3999 do
    a.(i) <- Word.empty ()
  done;
  a

let pp_memory f m =
  for i = 0 to 3999 do
    if not (Word.is_null m.(i)) then
      fprintf f "%4d : %a  --  %d\n" i pp_word m.(i) (to_int m.(i))
  done

let to_file m =
  let s = ref "" in
  for i = 0 to 3999 do
    s := !s ^ Word.to_file_format m.(i)
  done;
  !s

let of_file s =
  let m = empty () in
  let tmp = ref "" in
  let j = ref 0 in
  for i = 0 to String.length s - 1 do
    tmp := !tmp ^ String.make 1 s.[i];
    if (i + 1) mod 6 = 0 then begin
      Word.of_file_format m.(!j) !tmp;
      tmp := ""
    end
  done;
  m
