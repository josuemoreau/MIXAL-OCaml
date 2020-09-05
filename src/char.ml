
(* c is a lowercase letter, digit or symbol *)
let to_int c =
  match c with
  | ' '  -> 0
  | '.'  -> 40
  | ','  -> 41
  | '('  -> 42
  | ')'  -> 43
  | '+'  -> 44
  | '-'  -> 45
  | '*'  -> 46
  | '/'  -> 47
  | '='  -> 48
  | '$'  -> 49
  | '<'  -> 50
  | '>'  -> 51
  | '@'  -> 52
  | ';'  -> 53
  | ':'  -> 54
  | '\'' -> 55
  | '!'  -> 56
  | _    ->
    let n = int_of_char c in
    if int_of_char 'a' <= n && n <= int_of_char 'i' then
      1 + (n - int_of_char 'a')
    else if int_of_char 'j' <= n && n <= int_of_char 'r' then
      11 + (n - int_of_char 'j')
    else if int_of_char 's' <= n && n <= int_of_char 'z' then
      22 + (n - int_of_char 's')
    else if int_of_char '0' <= n && n <= int_of_char '9' then
      30 + (n - int_of_char '0')
    else
      failwith ("Caractère inconnu : " ^ String.make 1 c)

let of_int n =
  match n with
  | 0  -> ' '
  | 40 -> '.'
  | 41 -> ','
  | 42 -> '('
  | 43 -> ')'
  | 44 -> '+'
  | 45 -> '-'
  | 46 -> '*'
  | 47 -> '/'
  | 48 -> '='
  | 49 -> '$'
  | 50 -> '<'
  | 51 -> '>'
  | 52 -> '@'
  | 53 -> ';'
  | 54 -> ':'
  | 55 -> '\''
  | 56 -> '!'
  | _  ->
    if 1 <= n && n <= 9 then
      char_of_int (int_of_char 'a' + n - 1)
    else if 11 <= n && n <= 19 then
      char_of_int (int_of_char 'j' + n - 11)
    else if 22 <= n && n <= 29 then
      char_of_int (int_of_char 's' + n - 22)
    else if 30 <= n && n <= 39 then
      char_of_int (int_of_char '0' + n - 30)
    else
      failwith ("Code de caractère inconnu : " ^ string_of_int n)
