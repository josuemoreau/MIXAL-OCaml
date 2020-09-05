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
