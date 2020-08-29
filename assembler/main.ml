open Ast
open Word
open Format

module StringMap = Map.Make(String)

let bind_symbol (sym: Ast.symbol) (value: int)
    (bindings: int list StringMap.t) : int list StringMap.t =
  match sym with
  | SymLocal i ->
    let s = string_of_int i ^ "h" in
    StringMap.add s
      (value :: (try StringMap.find s bindings with Not_found -> []))
      bindings
  | SymString s ->
    StringMap.add s [value] bindings

let eval_symbol_loc line bindings sym =
  try
    match sym with
    | ESym s ->
      let l = StringMap.find s bindings in
      List.hd l
    | ELocalSymB i ->
      let l = StringMap.find (string_of_int i ^ "h") bindings in
      List.hd l
    | ELocalSymF i ->
      failwith ("Symbole " ^ string_of_int i ^ "f interdit dans l'adresse de cette instruction.")
    | _ -> assert (false)
  with Not_found ->
    failwith ("Symbol inconnu : " ^ match sym with
      | ESym s -> s
      | ELocalSymB i | ELocalSymF i -> string_of_int i
      | _ -> assert (false))

let eval_symbol_code line bindings sym =
  try
    match sym with
    | ESym s -> let _, l = StringMap.find s bindings in List.hd l
    | ELocalSymB i ->
      let p, l = StringMap.find (string_of_int i ^ "h") bindings in
      List.nth l (List.length l - p)
    | ELocalSymF i ->
      let p, l = StringMap.find (string_of_int i ^ "h") bindings in
      List.nth l (List.length l - p - 1)
    | _ -> assert (false)
  with
  | Not_found ->
    failwith ("Symbol inconnu : " ^ match sym with
      | ESym s -> s
      | ELocalSymB i | ELocalSymF i -> string_of_int i
      | _ -> assert (false))
  | Invalid_argument _ ->
    failwith ("Il n'y a pas de définition future du symbole : " ^ match sym with
      | ESym s -> s
      | ELocalSymB i | ELocalSymF i -> string_of_int i
      | _ -> assert (false))
  | Failure _ ->
    failwith ("Il n'y a pas de définition précédente du symbole : " ^ match sym with
      | ESym s -> s
      | ELocalSymB i | ELocalSymF i -> string_of_int i
      | _ -> assert (false))

let rec eval_expr eval_symbol line bindings = function
  | EPos a -> eval_atomicexpr eval_symbol line bindings a
  | ENeg a -> - (eval_atomicexpr eval_symbol line bindings a)
  | EBinary (BAdd, e, a)   ->
    ( + ) (eval_expr eval_symbol line bindings e) (eval_atomicexpr eval_symbol line bindings a)
  | EBinary (BSub, e, a)   ->
    ( - ) (eval_expr eval_symbol line bindings e) (eval_atomicexpr eval_symbol line bindings a)
  | EBinary (BMul, e, a)   ->
    ( * ) (eval_expr eval_symbol line bindings e) (eval_atomicexpr eval_symbol line bindings a)
  | EBinary (BDiv, e, a)   ->
    ( / ) (eval_expr eval_symbol line bindings e) (eval_atomicexpr eval_symbol line bindings a)
  | EBinary (BDivP, e, a)  ->
    ( fun a b -> a * word_size + b ) (eval_expr eval_symbol line bindings e) (eval_atomicexpr eval_symbol line bindings a)
  | EBinary (BFSpec, e, a) ->
    ( fun a b -> a * 8 + b ) (eval_expr eval_symbol line bindings e) (eval_atomicexpr eval_symbol line bindings a)
and eval_atomicexpr eval_symbol line bindings = function
  | ENum i -> i
  | EAsterisk -> line
  | sym -> eval_symbol line bindings sym

let rec eval_wval eval_symbol word line bindings = function
  | WAtomic a -> eval_atomicwval eval_symbol word line bindings a
  | WMul (w, a) ->
    eval_wval eval_symbol word line bindings w;
    eval_atomicwval eval_symbol word line bindings a
and eval_atomicwval eval_symbol word line bindings = function
  | WExpr (e, FEmpty) ->
    set_word_part line word (eval_expr eval_symbol line bindings e) 5
  | WExpr (e, FExpr f) ->
    set_word_part line word (eval_expr eval_symbol line bindings e)
      (eval_expr eval_symbol line bindings f)

let parse_loc =
  List.fold_left (fun (nbline, bindings) line ->
      match line with
      | SymDefInstr (sym, AssInstr i) when i.op = EQU ->
        let word = Word.empty () in
        eval_wval eval_symbol_loc word nbline bindings i.addr;
        (nbline, bind_symbol sym (Word.to_int word) bindings)
      | SymDefInstr (sym, AssInstr i) when i.op = ORIG ->
        let word = Word.empty () in
        eval_wval eval_symbol_loc word nbline bindings i.addr;
        let addr = Word.to_int word in
        (addr, bind_symbol sym nbline bindings)
      | SymDefInstr (sym, _) ->
        (nbline + 1, bind_symbol sym nbline bindings)
      | Instr (AssInstr i) when i.op = EQU -> (nbline, bindings)
      | Instr (AssInstr i) when i.op = ORIG ->
        let word = Word.empty () in
        eval_wval eval_symbol_loc word nbline bindings i.addr;
        (Word.to_int word, bindings)
      | Instr _ -> (nbline + 1, bindings)) (0, StringMap.empty)

let pp_intlist f l =
  let rec aux f l =
    match l with
    | [] -> ()
    | i :: [] -> fprintf f "%d" i
    | i :: l -> fprintf f "%d; %a" i aux l in
  fprintf f "[%a]\n" aux l

let pp_bindings f b =
  StringMap.iter (fun k l -> fprintf f "%s = %a" k pp_intlist l) b

let () =
  if Array.length Sys.argv > 1 then begin
    let c = open_in Sys.argv.(1) in
    let lb = Lexing.from_channel c in
    try
      let t = Parser.main Lexer.lexer lb in
      print_ast t;
      let nbline, bindings = parse_loc t in
      printf "%a@." pp_bindings bindings
    with _ ->
      let p = lb.lex_curr_p in
      Format.printf "Error : line %d, column %d" p.pos_lnum (p.pos_cnum - p.pos_bol)
  end else
    failwith "Argument non fourni"
