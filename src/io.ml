open Format
open Machine
open Word

let busy = Array.make 21 false

let block_size device =
  if 0 <= device && device <= 15 then 100
  else if device = 16 || device = 17 then 16
  else if device = 18 then 24
  else if device = 19 || device = 20 then 14
  else failwith ("Unknown device " ^ string_of_int device)

let ioc mach device m = ()

let input mach device m =
  while busy.(device) do () done;
  busy.(device) <- true;
  let line = read_line () in
  let lline = String.lowercase_ascii line in
  let set i n =
    let word_id = i / 5 in
    let byte_id = i mod 5 in
    set_byte (mem mach (m + word_id)) (byte_id + 1) n in
  for i = 0 to min (String.length lline - 1) (5 * block_size device - 1) do
    set i (Char.to_int lline.[i])
  done;
  busy.(device) <- false

let pp_device_block f (mach, device, m) =
  let bs = block_size device in
  let empi = ref 0 in
  for i = 0 to bs - 1 do
    let w = Machine.mem mach (m + i) in
    for j = 1 to 5 do
      if get_byte w j <> 0 then
        empi := i
    done
  done;
  for i = 0 to min !empi (bs - 1) do
    let w = Machine.mem mach (m + i) in
    for j = 1 to 5 do
      fprintf f "%c" (Char.of_int (get_byte w j))
    done
  done

let out mach device m =
  busy.(device) <- true;
  printf "%a@." pp_device_block (mach, device, m);
  busy.(device) <- false

let is_busy device = busy.(device)
