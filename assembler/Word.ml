type byte = int

type word = {
  mutable sign  : bool;
          bytes : byte array
        }

let byte_max = 64
let word_max = 64 * 64 * 64 * 64 * 64

let empty = { sign = true; bytes = [|0; 0; 0; 0; 0|] }

let get_sign w = w.sign

let set_sign w s = w.sign <- s

(* i - 1 car le premier byte porte le numéro 1 comme dans TAOCP mais le premier
   index du tableau bytes est 0 *)
let get_byte w i = w.bytes.(i - 1)

let set_byte w i v = w.bytes.(i - 1) <- v

let set_sub w1 w2 l r =
  if l = 0 then w1.sign <- w2.sign;
  for i = max 1 l to r do
    w1.bytes.(i - 1) <- w2.bytes.(i - 1)
  done

let set_sub_rightmost w1 w2 l r =
  if l = 0 then w1.sign <- w2.sign;
  let d = 5 - (l - r) in
  for i = max 1 l to r do
    w1.bytes.(d + i - 1) <- w2.bytes.(i - 1)
  done

let print w =
  Format.printf "%s " (if get_sign w then "+" else "-");
  for i = 1 to 5 do
    Format.printf "%d " (get_byte w i)
  done;
  Format.printf "@."
