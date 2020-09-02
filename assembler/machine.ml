open Format
open Word
open Memory

type comp = LESS | EQUAL | GREATER

type machine = {
  memory : memory;
  rI : word array;
  rJ : word;
  rA : word;
  rX : word;
  mutable overflow : bool;
  mutable comparison : comp;
  mutable loc_pointer : int
}

let init_rI () =
  let a = Array.make 6 (Word.empty ()) in
  for i = 1 to 5 do
    a.(i) <- Word.empty ()
  done;
  a

let init start_loc mem = {
  memory = mem;
  rI = init_rI ();
  rJ = Word.empty ();
  rA = Word.empty ();
  rX = Word.empty ();
  overflow = false;
  comparison = EQUAL;
  loc_pointer = start_loc
}

let mem mach i = mach.memory.(i)
let get_loc_pointer mach = mach.loc_pointer
let set_loc_pointer mach loc = mach.loc_pointer <- loc
let next_word mach = mach.memory.(mach.loc_pointer)

let get_rI mach i = mach.rI.(i - 1)
let get_int_rI mach i = get_word_part mach.rI.(i - 1) 5

let ld_rA mach m f = set_sub_f mach.rA mach.memory.(m) f
let ld_rI mach i m f = set_sub2_f (get_rI mach i) mach.memory.(m) f
let ld_rX mach m f = set_sub_f mach.rX mach.memory.(m) f

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
