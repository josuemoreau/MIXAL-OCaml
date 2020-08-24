
(* The type of tokens. *)

type token = 
  | STR of (string)
  | RPAR
  | PLUS
  | MUL
  | MIXOP of (string)
  | MINUS
  | LPAR
  | INT of (int)
  | FSPEC
  | EOF
  | DIVP
  | DIV
  | COMMA
  | ASTERISK
  | ASSOP of (string)
  | ALFOP

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val main: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (ast)
