open Format

type byte = int

type word = {
  mutable sign  : bool;
          bytes : byte array
}

let empty () = { sign = true; bytes = [|0; 0; 0; 0; 0|] }

let byte_size = 64

let word_size = byte_size * byte_size * byte_size * byte_size * byte_size

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

let base64 n =
  let rec aux acc n =
    if n < 64 then n :: acc
    else aux ((n mod 64) :: acc) (n / 64) in
  let sign = n >= 0 in
  (sign, aux [] (abs n))

let set_word_part line word n fspec =
  let (sign, parts) = base64 n in
  let size = List.length parts in
  let l, r = fspec / 8, fspec mod 8 in
  if 0 <= l && l <= 5 && l <= r && r <= 5 then begin
    if l = 0 then set_sign word sign;
    if r + 1 - (max l 1) >= size then
      List.iteri (fun i x -> set_byte word (r - (size - 1 - i)) x) parts
    else
      failwith ("La spécification F est trop petite à la ligne " ^ string_of_int line)
  end else
    failwith ("Mauvaise spécification F à la ligne " ^ string_of_int line)

let to_int word =
  let rec aux i =
    if i = 1 then get_byte word 1
    else get_byte word i + 64 * aux (i - 1) in
  if get_sign word then aux 5
  else - (aux 5)

let is_null w = w.sign && to_int w = 0

let pp_word f w =
  fprintf f "%s " (if get_sign w then "+" else "-");
  for i = 1 to 5 do
    fprintf f "%2d " (get_byte w i)
  done
