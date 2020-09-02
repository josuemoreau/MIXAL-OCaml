open Format
open Machine
open Word

let busy = Array.make 20 false

let block_size device =
  if 0 <= device && device <= 15 then 100
  else if device = 16 || device = 17 then 16
  else if device = 18 then 24
  else if device = 19 || device = 20 then 14
  else failwith ("Unknown device " ^ string_of_int device)

let ioc mach device m = ()

let out f mach device m =
  let bs = block_size device in
  for i = 0 to bs - 1 do
    let w = Machine.mem mach (m + i) in
    for j = 1 to 5 do
      fprintf f "%c" (char_of_int (get_byte w j))
    done
  done
