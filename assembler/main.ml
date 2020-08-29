open Ast
open Word

module StringMap = Map.Make(String)

type symbol = Int of int | Future of int

(* (\* On suppose la liste triée *\)
 * let rec find_local_symbol_before line = function
 *   | [] -> failwith "Symbole non défini"
 *   | loc :: [] -> if loc < line then loc else failwith "Symbole non défini"
 *   | locp :: loc :: symbols ->
 *     if locp < line && line <= loc then locp
 *     else find_local_symbol_before line (loc :: symbols)
 * 
 * (\* On suppose la liste triée *\)
 * let rec find_local_symbol_before line = function
 *   | [] -> failwith "Symbole non défini"
 *   | loc :: [] -> if loc < line then loc else failwith "Symbole non défini"
 *   | locp :: loc :: symbols ->
 *     if locp < line && line <= loc then locp
 *     else find_local_symbol_before line (loc :: symbols) *)

let bind_symbol (sym: Ast.symbol) (value: symbol)
    (bindings: symbol list StringMap.t) : symbol list StringMap.t =
  match sym with
  | SymLocal i ->
    let s = string_of_int i ^ "h" in
    StringMap.add s
      (value :: (try StringMap.find s bindings with Not_found -> []))
      bindings
  | SymString s ->
    StringMap.add s [value] bindings

let eval_symbol line bindings sym =
  try
    match sym with
    | ESym s ->
      let l = StringMap.find s bindings in
      List.hd l
    | ELocalSymB i ->
      let l = StringMap.find (string_of_int i ^ "h") bindings in
      List.hd l
    | ELocalSymF i ->
      let l = StringMap.find (string_of_int i ^ "h") bindings in
      Future (List.length l)
    | _ -> assert (false)
  with Not_found ->
    failwith ("Symbol inconnu : " ^ match sym with
      | ESym s -> s
      | ELocalSymB i | ELocalSymF i -> string_of_int i
      | _ -> assert (false))

let int_of_symbol = function
  | Int i -> i
  | _ -> failwith "Le symbole est une référence future."

let symbolop_of_op op a b =
  match a, b with
  | Int e1, Int e2 -> Int (op e1 e2)
  | _ -> failwith "Il est interdit d'utiliser un symbole futur avec des opérateurs arithmétiques."

let symadd   = symbolop_of_op ( + )
let symsub   = symbolop_of_op ( - )
let symmul   = symbolop_of_op ( * )
let symdiv   = symbolop_of_op ( / )
let symdivp  = symbolop_of_op ( fun a b -> a * word_size + b )
let symfspec = symbolop_of_op ( fun a b -> a * 8 + b )

let rec eval_expr line bindings = function
  | EPos a -> eval_atomicexpr line bindings a
  | ENeg a -> symsub (Int 0) (eval_atomicexpr line bindings a)
  | EBinary (BAdd, e, a)   ->
    symadd (eval_expr line bindings e) (eval_atomicexpr line bindings a)
  | EBinary (BSub, e, a)   ->
    symsub (eval_expr line bindings e) (eval_atomicexpr line bindings a)
  | EBinary (BMul, e, a)   ->
    symmul (eval_expr line bindings e) (eval_atomicexpr line bindings a)
  | EBinary (BDiv, e, a)   ->
    symdiv (eval_expr line bindings e) (eval_atomicexpr line bindings a)
  | EBinary (BDivP, e, a)  ->
    symdivp (eval_expr line bindings e) (eval_atomicexpr line bindings a)
  | EBinary (BFSpec, e, a) ->
    symfspec (eval_expr line bindings e) (eval_atomicexpr line bindings a)
and eval_atomicexpr line bindings = function
  | ENum i -> Int i
  | EAsterisk -> Int line
  | sym -> eval_symbol line bindings sym

let rec eval_wval word line bindings = function
  | WAtomic a -> eval_atomicwval word line bindings a
  | WMul (w, a) ->
    eval_wval word line bindings w;
    eval_atomicwval word line bindings a
and eval_atomicwval word line bindings = function
  | WExpr (e, FEmpty) ->
    set_word_part line word (int_of_symbol (eval_expr line bindings e)) 5
  | WExpr (e, FExpr f) ->
    set_word_part line word (int_of_symbol (eval_expr line bindings e))
      (int_of_symbol (eval_expr line bindings f))

let parse_loc =
  List.fold_left (fun (nbline, bindings) line ->
      match line with
      | SymDefInstr (sym, AssInstr i) when i.op = EQU ->
        let word = Word.empty () in
        eval_wval word nbline bindings i.addr;
        (nbline, bind_symbol sym (Int (Word.to_int word)) bindings)
      | SymDefInstr (sym, AssInstr i) when i.op = ORIG ->
        let word = Word.empty () in
        eval_wval word nbline bindings i.addr;
        let addr = Word.to_int word in
        (addr + 1, bind_symbol sym (Int addr) bindings)
      | SymDefInstr (sym, _) ->
        (nbline + 1, bind_symbol sym (Int nbline) bindings)
      | Instr (AssInstr i) when i.op = EQU -> (nbline, bindings)
      | Instr (AssInstr i) when i.op = ORIG ->
        let word = Word.empty () in
        eval_wval word nbline bindings i.addr;
        (Word.to_int word + 1, bindings)
      | Instr _ -> (nbline + 1, bindings)) (0, StringMap.empty)

let () =
  if Array.length Sys.argv > 1 then begin
    let c = open_in Sys.argv.(1) in
    let lb = Lexing.from_channel c in
    try
      let t = Parser.main Lexer.lexer lb in
      print_ast t;
      let nbline, bindings = parse_loc t in
      StringMap.iter (fun k v -> Format.printf "%s = " k v) bindings
    with _ ->
      let p = lb.lex_curr_p in
      Format.printf "Error : line %d, column %d" p.pos_lnum (p.pos_cnum - p.pos_bol)
  end else
    failwith "Argument non fourni"
