open Format
open Word
open Memory

type comp = LESS | EQUAL | GREATER

type machine = {
          memory      : memory;
          rI          : word array;
          rJ          : word;
          rA          : word;
          rX          : word;
  mutable overflow    : bool;
  mutable comp        : comp;
  mutable loc_pointer : int
}

let init_rI () =
  let a = Array.make 6 (Word.empty ()) in
  for i = 1 to 5 do
    a.(i) <- Word.empty ()
  done;
  a

let init start_loc mem = {
  memory      = mem;
  rI          = init_rI ();
  rJ          = Word.empty ();
  rA          = Word.empty ();
  rX          = Word.empty ();
  overflow    = false;
  comp        = EQUAL;
  loc_pointer = start_loc
}

let mem mach i = mach.memory.(i)
let get_loc_pointer mach = mach.loc_pointer
let set_loc_pointer mach loc = mach.loc_pointer <- loc
let next_word mach = mach.memory.(mach.loc_pointer)

let get_rI mach i = mach.rI.(i - 1)
let get_int_rI mach i = get_word_part mach.rI.(i - 1) 5

let add mach m f =
  let v1 = get_word_part mach.rA 5 in
  let v2 = get_word_part mach.memory.(m) f in
  try
    set_word_part 0 mach.rA (v1 + v2) 5
  with Overflow ->
    mach.overflow <- true

let sub mach m f = add mach (-m) f

let mul mach m f =
  let v1 = get_word_part mach.rA 5 in
  let v2 = get_word_part mach.memory.(m) f in
  let prod = v1 * v2 in
  let sign = if prod >= 0 then 1 else -1 in
  let abs_prod = abs prod in
  let rA_val = abs_prod / word_size in
  let rX_val = abs_prod mod word_size in
  let rA_val' =
    if rA_val < word_size then rA_val
    else begin
      mach.overflow <- true;
      rA_val mod word_size
    end in
  set_word_part 0 mach.rA (sign * rA_val') 5;
  set_word_part 0 mach.rX (sign * rX_val) 5

let div mach m f =
  let vA = abs (to_int mach.rA) in
  let vX = abs (to_int mach.rX) in
  let v1 = (if get_sign mach.rA then 1 else -1) * (vA * word_size + vX) in
  let v2 = get_word_part mach.memory.(m) f in
  let q = v1 / v2 in
  let r = v1 mod v2 in
  if abs q >= word_size then mach.overflow <- true
  else begin
    set_word_part 0 mach.rA q 5;
    set_word_part 0 mach.rX r 5
  end



let ld_rA mach m f = set_sub_shift_f mach.rA mach.memory.(m) f
let ld_rI mach i m f = set_sub_shift_f (get_rI mach i) mach.memory.(m) f
let ld_rX mach m f = set_sub_shift_f mach.rX mach.memory.(m) f

let ldn dest src f =
  set_sub_f dest src f;
  let sign = get_sign src in
  set_sign dest (not sign)
let ldn_rA mach m f = ldn mach.rA mach.memory.(m) f
let ldn_rX mach m f = ldn mach.rX mach.memory.(m) f
let ldn_rI mach i m f =
  ld_rI mach i m f;
  let sign = get_sign mach.memory.(m) in
  set_sign (get_rI mach i) (not sign)

let st_rA mach m f = set_sub_f mach.memory.(m) mach.rA f
let st_rI mach i m f = set_sub2_f mach.memory.(m) (get_rI mach i) f
let st_rX mach m f = set_sub_f mach.memory.(m) mach.rX f

let ent dest sign m =
  if m = 0 then set_sign dest sign;
  set_word_part 0 dest m 5
let ent_rA mach sign m = ent mach.rA sign m
let ent_rI mach i sign m =
  let rI = get_rI mach i in
  if m = 0 then set_sign rI sign;
  set_word_part 0 rI m (4 * 8 + 5)
let ent_rX mach sign m = ent mach.rA sign m

let enn_rA mach sign m = ent_rA mach sign (-m)
let enn_rI mach i sign m = ent_rI mach i sign (-m)
let enn_rX mach sign m = ent_rX mach sign (-m)

let inc dest d =
  let n = to_int dest in
  set_word_part 0 dest (n + d) 5
let inc_rA mach m = inc mach.rA m
let inc_rI mach i m =
  let n = get_int_rI mach i in
  set_word_part 0 (get_rI mach i) (n + m) (4 * 8 + 5)
let inc_rX mach m = inc mach.rX m

let dec_rA mach m = inc_rA mach (-m)
let dec_rI mach i m = inc_rI mach i (-m)
let dec_rX mach m = inc mach.rX (-m)

let is_z src = to_int src = 0
let is_p src = to_int src > 0
let is_n src = to_int src < 0
let is_nz src = to_int src <> 0
let is_np src = to_int src <= 0
let is_nn src = to_int src >= 0

let is2_z src = to_int2 src = 0
let is2_p src = to_int2 src > 0
let is2_n src = to_int2 src < 0
let is2_nz src = to_int2 src <> 0
let is2_np src = to_int2 src <= 0
let is2_nn src = to_int2 src >= 0

let j_rA mach m comp =
  if comp mach.rA then m
  else mach.loc_pointer + 1
let j_rI mach i m comp =
  if comp mach.rI.(i) then m
  else mach.loc_pointer + 1
let j_rX mach m comp =
  if comp mach.rX then m
  else mach.loc_pointer + 1

let cmp mach w1 w2 f =
  let v1 = get_word_part w1 f in
  let v2 = get_word_part w2 f in
  if v1 < v2 then mach.comp <- LESS
  else if v1 = v2 then mach.comp <- EQUAL
  else mach.comp <- GREATER
let cmp_rA mach m f = cmp mach mach.rA mach.memory.(m) f
let cmp_rI mach i m f =
  let v1 = get_word_part2 mach.rI.(i) f in
  let v2 = get_word_part2 mach.memory.(m) f in
  if v1 < v2 then mach.comp <- LESS
  else if v1 = v2 then mach.comp <- EQUAL
  else mach.comp <- GREATER
let cmp_rX mach m f = cmp mach mach.rX mach.memory.(m) f
