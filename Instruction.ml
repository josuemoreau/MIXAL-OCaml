open Word
open Op

type instr = {
  op      : op;
  address : int;
  index   : int;
  fspec   : int
}

exception InvalidFSpec of int
exception AddressSpecifiedTooLarge of int

let addr_to_bytes addr =
  let rec aux addr acc =
    let q = addr / 64 in
    let r = addr mod 64 in
    if q = 0 then r :: acc
    else aux q (r :: acc) in
  aux addr []

let to_word op addr i f line =
  let op_spec = op_to_codes op in
  let w = empty in
  let bytes = addr_to_bytes (abs addr) in
  if List.length bytes > 2 then raise (AddressSpecifiedTooLarge line);
  set_sign w (addr >= 0);
  set_byte w 2 (List.nth bytes 0);
  if List.length bytes >= 2 then set_byte w 1 (List.nth bytes 1);
  begin match i with
    | None -> () (* pas besoin de définir le byte, il est déjà initialisé à 0 *)
    | Some(i) -> set_byte w 3 i end;
  begin
    match f with
    | None -> set_byte w 4 op_spec.op_normal
    | Some(f) -> begin
        match op_spec.op_variant with
        | AnyLR    ->
          if is_fspec05_valid f then set_byte w 4 f
          else raise (InvalidFSpec line)
        | AnyLR02  ->
          if is_fspec02_valid f then set_byte w 4 f
          else raise (InvalidFSpec line)
        | AnyFrom1 ->
          if f = 0 then raise (InvalidFSpec line)
          else set_byte w 4 f
        | Int i    -> set_byte w 4 i
      end
  end;
  set_byte w 5 op_spec.op_code;
  w
