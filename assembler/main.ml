open Ast

let () =
  if Array.length Sys.argv > 1 then begin
    let c = open_in Sys.argv.(1) in
    let lb = Lexing.from_channel c in
    try
      let t = Parser.main Lexer.lexer lb in
      print_ast t
    with _ ->
      let p = lb.lex_curr_p in
      Format.printf "Error : line %d, column %d" p.pos_lnum (p.pos_cnum - p.pos_bol)
  end else
    failwith "Argument non fourni"
