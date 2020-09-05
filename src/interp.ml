open Format
open Word
open Machine
open Io

exception End

let invalid_fspec i = failwith ("F-Spécification invalide." ^ string_of_int i)

let f_to_comp f =
  match f with
  | 0 -> is_n
  | 1 -> is_z
  | 2 -> is_p
  | 3 -> is_nn
  | 4 -> is_nz
  | 5 -> is_np
  | _ -> invalid_fspec f

let f_to_comp2 f =
  match f with
  | 0 -> is2_n
  | 1 -> is2_z
  | 2 -> is2_p
  | 3 -> is2_nn
  | 4 -> is2_nz
  | 5 -> is2_np
  | _ -> invalid_fspec f

let exec_instr mach =
  let w        = next_word mach in
  let c        = get_byte w 5 in
  let f        = get_byte w 4 in
  let index    = get_byte w 3 in
  let addr     = get_word_part w 2 in
  let sign     = get_sign w in
  let m        =
    if index = 0 then addr
    else if 1 <= index && index <= 6 then addr + get_int_rI mach index
    else failwith ("Le champ I a une valeur invalide : " ^ string_of_int index) in
  let next_loc = ref (1 + get_loc_pointer mach) in
  (* printf "%d - INSTR : C = %2d ; M = %4d ; I = %d ; F = %2d@." mach.loc_pointer c m index f; *)
  if 9 <= c && c <= 14 then ld_rI mach (c - 8) m f
  else if 17 <= c && c <= 22 then ldn_rI mach (c - 16) m f
  else if 25 <= c && c <= 30 then st_rI mach (c - 24) m f
  else if 41 <= c && c <= 46 then
    next_loc := (j_rI mach (c - 40) m (f_to_comp2 f))
  else if 49 <= c && c <= 54 then
    begin match f with
      | 0 -> inc_rI mach (c - 48) m
      | 1 -> dec_rI mach (c - 48) m
      | 2 -> ent_rI mach (c - 48) sign m
      | 3 -> enn_rI mach (c - 48) sign m
      | _ -> invalid_fspec f
    end
  else if 57 <= c && c <= 62 then cmp_rI mach (c - 56) m f
  else
    begin match c with
      | 0  -> ()
      | 1  -> add mach m f
      | 2  -> sub mach m f
      | 3  -> mul mach m f
      | 4  -> div mach m f
      | 5  ->
        begin match f with
          | 0 -> num mach
          | 1 -> char mach
          | 2 -> raise End
          | _ -> invalid_fspec f
        end
      | 6  ->
        begin match f with
          | 0 -> sla mach m
          | 1 -> sra mach m
          | 2 -> slax mach m
          | 3 -> srax mach m
          | 4 -> slc mach m
          | 5 -> src mach m
          | _ -> invalid_fspec f
        end
      | 7  -> move mach m f
      | 8  -> ld_rA mach m f
      | 15 -> ld_rX mach m f
      | 16 -> ldn_rA mach m f
      | 23 -> ldn_rX mach m f
      | 24 -> st_rA mach m f
      | 31 -> st_rX mach m f
      | 32 -> st_rJ mach m f
      | 33 -> st_z mach m f
      | 34 -> if is_busy f then next_loc := m
      | 35 -> ioc mach f m
      | 36 -> input mach f m
      | 37 -> out mach f m
      | 38 -> if not (is_busy f) then next_loc := m
      | 39 ->
        begin match f with
          | 0 -> next_loc := jmp mach m
          | 1 -> next_loc := jsj mach m
          | 2 -> next_loc := jov mach m
          | 3 -> next_loc := jnov mach m
          | 4 -> next_loc := jl mach m
          | 5 -> next_loc := je mach m
          | 6 -> next_loc := jg mach m
          | 7 -> next_loc := jge mach m
          | 8 -> next_loc := jne mach m
          | 9 -> next_loc := jle mach m
          | _ -> invalid_fspec f
        end
      | 40 -> next_loc := (j_rA mach m (f_to_comp f))
      | 47 -> next_loc := (j_rX mach m (f_to_comp f))
      | 48 ->
        begin match f with
          | 0 -> inc_rA mach m
          | 1 -> dec_rA mach m
          | 2 -> ent_rA mach sign m
          | 3 -> enn_rA mach sign m
          | _ -> invalid_fspec f
        end
      | 55 ->
        begin match f with
          | 0 -> inc_rX mach m
          | 1 -> dec_rX mach m
          | 2 -> ent_rX mach sign m
          | 3 -> enn_rX mach sign m
          | _ -> invalid_fspec f
        end
      | 56 -> cmp_rA mach m f
      | 63 -> cmp_rX mach m f
      | _  -> ()
    end;
  set_loc_pointer mach !next_loc

let exec mach =
  try
    while true do
      (* if !step then ignore (read_line ()); *)
      exec_instr mach;
      (* printf "%a@." pp_registers mach;
       * printf "%a@." pp_indicators mach;
       * printf "---------------------------------------------------------------@." *)
      (* printf "%a" Memory.pp_memory mach.memory *)
    done
  with
  | End ->
    (* printf "%a@." Memory.pp_memory mach.memory; *)
    printf "END OF PROGRAM"
  | Overflow -> printf "%a@." Machine.pp_memory mach
