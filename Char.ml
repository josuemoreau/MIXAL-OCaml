type char = string

module MapString = Map.Make(String)

let chars = [|
  " "; "A"; "B"; "C"; "D"; "E"; "F"; "G"; "H"; "I";
  "\u{0394}"; "J"; "K"; "L"; "M"; "N"; "O"; "P"; "Q"; "R";
  "\u{03A3}"; "\u{03A0}"; "S"; "T"; "U"; "V"; "W"; "X"; "Y"; "Z";
  "0"; "1"; "2"; "3"; "4"; "5"; "6"; "7"; "8"; "9";
  "."; ","; "("; ")"; "+"; "-"; "*"; "/"; "="; "$";
  "<"; ">"; "@"; ";"; ":"; "'"
|]

let switch_seq = Seq.map (fun (i, j) -> (j, i))

let charcodes = MapString.of_seq (switch_seq (Array.to_seqi chars))

let char_of_int i = chars.(i)
let int_of_char c = MapString.find c charcodes
