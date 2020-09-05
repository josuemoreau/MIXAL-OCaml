open Format

type byte = int

type word = {
  mutable sign  : bool;
          bytes : byte array
        }

exception Overflow

let empty () = { sign = true; bytes = [|0; 0; 0; 0; 0|] }

let byte_size = 64

let word_size = byte_size * byte_size * byte_size * byte_size * byte_size

let get_sign w = w.sign

let set_sign w s = w.sign <- s

(* i - 1 car le premier byte porte le numéro 1 comme dans TAOCP mais le premier
   index du tableau bytes est 0 *)
let get_byte w i = w.bytes.(i - 1)

let set_byte w i v = w.bytes.(i - 1) <- v

let pp_word f w =
  fprintf f "%s " (if get_sign w then "+" else "-");
  for i = 1 to 5 do
    fprintf f "%2d " (get_byte w i)
  done

let base64 n =
  let rec aux acc n =
    if n < 64 then n :: acc
    else aux ((n mod 64) :: acc) (n / 64) in
  let sign = n >= 0 in
  (sign, aux [] (abs n))

let rec int_pow x n =
  if n = 0 then 1
  else
    let y = int_pow x (n / 2) in
    if n mod 2 = 0 then y * y
    else y * y * x

let set_zero word fspec =
  let l, r = fspec / 8, fspec mod 8 in
  if l = 0 then set_sign word true;
  for i = max 1 l - 1 to r - 1 do
    word.bytes.(i) <- 0
  done

let set_word_part line word n fspec =
  (* if n >= word_size || n <= -word_size then raise Overflow; *)
  set_zero word fspec;
  let (sign, parts) = base64 n in
  let size = List.length parts in
  let l, r = fspec / 8, fspec mod 8 in
  if 0 <= l && l <= 5 && l <= r && r <= 5 then begin
    if l = 0 then set_sign word sign;
    if r + 1 - (max l 1) >= size then
      List.iteri (fun i x -> set_byte word (r - (size - 1 - i)) x) parts
    else begin
      let (_, parts) = base64 (n mod (int_pow 64 (r + 1 - max l 1))) in
      let size = List.length parts in
      List.iteri (fun i x -> set_byte word (r - (size - 1 - i)) x) parts;
      raise Overflow
    end
  end else
    failwith ("Mauvaise spécification F à la ligne " ^ string_of_int line)

let set_word_part2 line word n =
  set_sign word (n >= 0);
  set_word_part line word n (4 * 8 + 5)

let set_sub w1 w2 l r =
  if l = 0 then w1.sign <- w2.sign;
  for i = max 1 l to r do
    w1.bytes.(i - 1) <- w2.bytes.(i - 1)
  done

let set_sub_f w1 w2 f =
  let l, r = f / 8, f mod 8 in
  set_sub w1 w2 l r

let set_sub2_f w1 w2 f =
  let l, r = f / 8, f mod 8 in
  if l = 0 then w1.sign <- w2.sign;
  if l <= 4 && 4 <= r then w1.bytes.(3) <- w2.bytes.(3);
  if l <= 5 && 5 <= r then w1.bytes.(4) <- w2.bytes.(4)

let set_sub_shift w1 w2 l r =
  if l = 0 then w1.sign <- w2.sign;
  let l' = max 1 l in
  let d = 5 - (r - l') in
  for i = 1 to d - 1 do
    set_byte w1 i 0
  done;
  for i = 0 to r - l' do
    set_byte w1 (d + i) (get_byte w2 (l' + i))
  done

let set_sub_shift_bis w1 w2 l r =
  if l = 0 then w1.sign <- w2.sign;
  let l' = max 1 l in
  let d = 5 - (r - l') in
  for i = 1 to d - 1 do
    set_byte w1 i 0
  done;
  for i = 0 to r - l' do
    set_byte w1 (l' + i) (get_byte w2 (d + i))
  done

let set_sub_shift_bis_f w1 w2 f =
  let l, r = f / 8, f mod 8 in
  set_sub_shift_bis w1 w2 l r

let set_sub_shift_f w1 w2 f =
  let l, r = f / 8, f mod 8 in
  set_sub_shift w1 w2 l r

let set_sub_shift_bis2_f w1 w2 f =
  for i = 0 to 2 do
    w2.bytes.(i) <- 0
  done;
  set_sub_shift_bis_f w1 w2 f

let to_int word =
  let rec aux i =
    if i = 1 then get_byte word 1
    else get_byte word i + 64 * aux (i - 1) in
  if get_sign word then aux 5
  else - (aux 5)

let get_word_part word fspec =
  let l, r = fspec / 8, fspec mod 8 in
  if 0 <= l && l <= 5 && l <= r && r <= 5 then begin
    let n = ref 0 in
    for i = max 1 l to r do
      n := 64 * !n + get_byte word i
    done;
    if l = 0 then
      if get_sign word then !n
      else (-1) * !n
    else !n
  end else
    failwith ("Mauvaise spécification F")

let get_word_part2 word fspec =
  let l, r = fspec / 8, fspec mod 8 in
  if 0 <= l && l <= 5 && l <= r && 4 <= r && r <= 5 then begin
    let n = ref 0 in
    for i = max 4 l to r do
      n := 64 * !n + get_byte word i
    done;
    if l = 0 then
      if get_sign word then !n
      else (-1) * !n
    else !n
  end else
    failwith ("Mauvaise spécification F")

let to_int2 word =
  (* let n = get_word_part word (4 * 8 + 5) in
   * let sign = get_sign word in
   * if sign then n
   * else -n *)
  get_word_part2 word 5

let is_null w = w.sign && to_int w = 0

(* let inc w d =
 *   let i = ref 5 in
 *   let r = ref d in
 *   while !r > 0 do
 *     if !i = 0 then raise Overflow;
 *     let v = get_byte w !i in
 *     if v + !r >= 64 then begin
 *       set_byte w !i ((v + !r) mod 64);
 *       r := (v + !r) / 64
 *     end else begin
 *       set_byte w !i (v + !r);
 *       r := 0
 *     end
 *   done *)
