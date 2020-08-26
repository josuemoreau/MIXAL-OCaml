
(* The type of tokens. *)

type token = 
  | RPAR
  | PLUS
  | MUL
  | MIXOP of (string)
  | MINUS
  | LPAR
  | INT of (int)
  | IDENT of (string)
  | FSPEC
  | EQUAL
  | EOF
  | EINSTR
  | DIVP
  | DIV
  | COMMA
  | ASTERISK
  | ASSOP of (string)
  | ALFOP of (string)

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val main: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.ast)
