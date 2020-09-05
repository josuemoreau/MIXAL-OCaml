open Format
open Word
open Memory

type machine = {
          memory       : memory;
          rI           : word array;
          rJ           : word;
          rA           : word;
          rX           : word;
  mutable overflow     : bool;
  mutable comp_less    : bool;
  mutable comp_equal   : bool;
  mutable comp_greater : bool;
  mutable loc_pointer  : int
}

let init_rI () =
  let a = Array.make 6 (Word.empty ()) in
  for i = 1 to 5 do
    a.(i) <- Word.empty ()
  done;
  a

let init start_loc mem = {
  memory       = mem;
  rI           = init_rI ();
  rJ           = Word.empty ();
  rA           = Word.empty ();
  rX           = Word.empty ();
  overflow     = false;
  comp_equal   = false;
  comp_less    = false;
  comp_greater = false;
  loc_pointer  = start_loc
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

let char mach =
  let n = ref (to_int mach.rA) in
  for i = 5 downto 1 do
    let r = !n mod 10 in
    set_byte mach.rX i (30 + r);
    n := !n / 10
  done;
  for i = 5 downto 1 do
    let r = !n mod 10 in
    set_byte mach.rA i (30 + r);
    n := !n / 10
  done

let num mach =
  let n = ref 0 in
  for i = 1 to 5 do
    n := !n * 10 + get_byte mach.rA i - 30
  done;
  for i = 1 to 5 do
    n := !n * 10 + get_byte mach.rX i - 30
  done;
  set_word_part 0 mach.rA !n 5

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

let st_rA mach m f = set_sub_shift_bis_f mach.memory.(m) mach.rA f
let st_rI mach i m f = set_sub_shift_bis2_f mach.memory.(m) (get_rI mach i) f
let st_rX mach m f = set_sub_shift_bis_f mach.memory.(m) mach.rX f
let st_rJ mach m f =
  set_sign mach.memory.(m) (get_sign mach.rJ);
  set_sub_shift_bis2_f mach.memory.(m) mach.rJ f
let st_z mach m f = set_zero mach.memory.(m) f

let ent dest sign m =
  if m = 0 then set_sign dest sign
  else set_sign dest (m >= 0);
  set_word_part 0 dest m 5
let ent_rA mach sign m = ent mach.rA sign m
let ent_rI mach i sign m =
  let rI = get_rI mach i in
  set_word_part2 0 rI m;
  if m = 0 then set_sign rI sign
let ent_rX mach sign m = ent mach.rX sign m

let enn_rA mach sign m = ent_rA mach sign (-m)
let enn_rI mach i sign m = ent_rI mach i sign (-m)
let enn_rX mach sign m = ent_rX mach sign (-m)

let inc dest d =
  let n = to_int dest in
  set_word_part 0 dest (n + d) 5
let inc_rA mach m = inc mach.rA m
let inc_rI mach i m =
  let n = get_int_rI mach i in
  set_word_part2 0 (get_rI mach i) (n + m)
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

let jmp mach m =
  set_word_part 0 mach.rJ (mach.loc_pointer + 1) 5;
  m
let jsj mach m = m

let j_rA mach m comp =
  if comp mach.rA then jmp mach m
  else mach.loc_pointer + 1
let j_rI mach i m comp =
  if comp (get_rI mach i) then jmp mach m
  else mach.loc_pointer + 1
let j_rX mach m comp =
  if comp mach.rX then jmp mach m
  else mach.loc_pointer + 1

let j_cond mach m cond =
  if cond then jmp mach m
  else mach.loc_pointer + 1
let jov mach m = j_cond mach m mach.overflow
let jnov mach m = j_cond mach m (not mach.overflow)
let jl mach m = j_cond mach m mach.comp_less
let je mach m = j_cond mach m mach.comp_equal
let jg mach m = j_cond mach m mach.comp_greater
let jge mach m = j_cond mach m (mach.comp_greater || mach.comp_equal)
let jne mach m = j_cond mach m (mach.comp_greater || mach.comp_less)
let jle mach m = j_cond mach m (mach.comp_less || mach.comp_equal)

let cmp mach w1 w2 f =
  let v1 = get_word_part w1 f in
  let v2 = get_word_part w2 f in
  (* Format.printf "============================\n\n\n\n\nCMPA : %d %d@." v1 v2; *)
  if v1 < v2 then begin mach.comp_less <- true; mach.comp_equal <- false; mach.comp_greater <- false end
  else if v1 = v2 then begin mach.comp_equal <- true; mach.comp_less <- false; mach.comp_greater <- false end
  else begin mach.comp_greater <- true; mach.comp_less <- false; mach.comp_equal <- false end
let cmp_rA mach m f = cmp mach mach.rA mach.memory.(m) f
let cmp_rI mach i m f =
  let v1 = get_word_part2 (get_rI mach i) f in
  let v2 = get_word_part2 mach.memory.(m) f in
  (* Format.printf "============================\n\n\n\n\nCMP%d : %d %d@." i v1 v2; *)
  if v1 < v2 then begin mach.comp_less <- true; mach.comp_equal <- false; mach.comp_greater <- false end
  else if v1 = v2 then begin mach.comp_equal <- true; mach.comp_less <- false; mach.comp_greater <- false end
  else begin mach.comp_greater <- true; mach.comp_less <- false; mach.comp_equal <- false end
let cmp_rX mach m f = cmp mach mach.rX mach.memory.(m) f

let sla mach m =
  if m >= 5 then set_zero mach.rA 5
  else begin
    for i = 1 to 5 - m do
      set_byte mach.rA i (get_byte mach.rA (m + i))
    done;
    for i = 5 - m + 1 to 5 do
      set_byte mach.rA i 0
    done
  end

let sra mach m =
  if m >= 5 then set_zero mach.rA 5
  else begin
    for i = 5 downto m + 1 do
      set_byte mach.rA i (get_byte mach.rA (i - m))
    done;
    for i = 1 to m do
      set_byte mach.rA i 0
    done
  end

let slax mach m =
  if m >= 10 then begin set_zero mach.rA 5; set_zero mach.rX 5 end
  else begin
    let set i v =
      if 1 <= i && i <= 5 then set_byte mach.rA i v
      else set_byte mach.rX (i - 5) v in
    let get i =
      if 1 <= i && i <= 5 then get_byte mach.rA i
      else get_byte mach.rX (i - 5) in
    for i = 1 to 10 - m do
      set i (get (m + i))
    done;
    for i = 10 - m + 1 to 10 do
      set i 0
    done
  end

let srax mach m =
  if m >= 10 then begin set_zero mach.rA 5; set_zero mach.rX 5 end
  else begin
    let set i v =
      if 1 <= i && i <= 5 then set_byte mach.rA i v
      else set_byte mach.rX (i - 5) v in
    let get i =
      if 1 <= i && i <= 5 then get_byte mach.rA i
      else get_byte mach.rX (i - 5) in
    for i = 10 downto m + 1 do
      set i (get (i - m))
    done;
    for i = 1 to m do
      set i 0
    done
  end

let slc mach m =
  let m = m mod 10 in
  let temp_rA = Word.empty () in
  let temp_rX = Word.empty () in
  let set i v =
    if 1 <= i && i <= 5 then set_byte temp_rA i v
    else set_byte temp_rX (i - 5) v in
  let get i =
    if 1 <= i && i <= 5 then get_byte mach.rA i
    else get_byte mach.rX (i - 5) in
  for i = 1 to 10 do
    let j = ((i + m - 1) mod 10) + 1 in
    set i (get j)
  done;
  for i = 1 to 5 do
    set_byte mach.rA i (get_byte temp_rA i);
    set_byte mach.rX i (get_byte temp_rX i)
  done

let src mach m =
  let m = m mod 10 in
  let temp_rA = Word.empty () in
  let temp_rX = Word.empty () in
  let set i v =
    if 1 <= i && i <= 5 then set_byte temp_rA i v
    else set_byte temp_rX (i - 5) v in
  let get i =
    if 1 <= i && i <= 5 then get_byte mach.rA i
    else get_byte mach.rX (i - 5) in
  for i = 1 to 10 do
    let j = ((10 + i - m - 1) mod 10) + 1 in
    set i (get j)
  done;
  for i = 1 to 5 do
    set_byte mach.rA i (get_byte temp_rA i);
    set_byte mach.rX i (get_byte temp_rX i)
  done

(* à implémenter pour l'exemple des 500 primes : OUT, CHAR *)
