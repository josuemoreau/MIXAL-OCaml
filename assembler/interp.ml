open Word
open Machine
open Io

let exec_instr mach =
  let w        = next_word mach in
  let c        = get_byte w 5 in
  let f        = get_byte w 4 in
  let index    = get_byte w 3 in
  let addr     = get_word_part w 2 in
  let sign     = get_sign w in
  let m        =
    if index = 0 then 0
    else if 1 <= index && index <= 6 then addr + get_int_rI mach index
    else failwith ("Le champ I a une valeur invalide : " ^ string_of_int index) in
  let next_loc = ref (1 + get_loc_pointer mach) in
  Format.printf "INSTR : C = %2d ; M = %4d ; I = %d ; F = %2d@." c m index f;
  if 9 <= c && c <= 14 then ld_rI mach (c - 8) m f
  else if 17 <= c && c <= 22 then ldn_rI mach (c - 16) m f
  else if 49 <= c && c <= 54 then
    begin match f with
      | 0 -> inc_rI mach (c - 48) m
      | 1 -> dec_rI mach (c - 48) m
      | 2 -> ent_rI mach (c - 48) sign m
      | 3 -> enn_rI mach (c - 48) sign m
      | _ -> failwith "F-Spécification invalide."
    end
  else
    begin match c with
      | 0  -> ()
      | 8  -> ld_rA mach m f
      | 15 -> ld_rX mach m f
      | 16 -> ldn_rA mach m f
      | 23 -> ldn_rX mach m f
      | 48 ->
        begin match f with
          | 0 -> inc_rA mach m
          | 1 -> dec_rA mach m
          | 2 -> ent_rA mach sign m
          | 3 -> enn_rA mach sign m
          | _ -> failwith "F-Spécification invalide."
        end
      | 55 ->
        begin match f with
          | 0 -> inc_rX mach m
          | 1 -> dec_rX mach m
          | 2 -> ent_rX mach sign m
          | 3 -> enn_rX mach sign m
          | _ -> failwith "F-Spécification invalide."
        end
      | 35 -> ioc mach f m
      | _  -> ()
    end;
  set_loc_pointer mach !next_loc

let exec mach =
  for i = 1 to 4 do
    exec_instr mach
  done
