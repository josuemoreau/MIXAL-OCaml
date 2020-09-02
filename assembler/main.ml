open Ast
open Word
open Format
open Memory
open Instr
open Machine

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
    eval_expr eval_symbol line bindings e + eval_atomicexpr eval_symbol line bindings a
  | EBinary (BSub, e, a)   ->
    eval_expr eval_symbol line bindings e - eval_atomicexpr eval_symbol line bindings a
  | EBinary (BMul, e, a)   ->
    eval_expr eval_symbol line bindings e * eval_atomicexpr eval_symbol line bindings a
  | EBinary (BDiv, e, a)   ->
    eval_expr eval_symbol line bindings e / eval_atomicexpr eval_symbol line bindings a
  | EBinary (BDivP, e, a)  ->
    eval_expr eval_symbol line bindings e * word_size + eval_atomicexpr eval_symbol line bindings a
  | EBinary (BFSpec, e, a) ->
    eval_expr eval_symbol line bindings e * 8 + eval_atomicexpr eval_symbol line bindings a
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

let eval_addr eval_symbol line bindings = function
  | AEmpty -> 0
  | AExpr e -> eval_expr eval_symbol line bindings e
  | ALiteral l -> 0

let eval_index eval_symbol line bindings = function
  | IEmpty -> 0
  | IExpr e -> eval_expr eval_symbol line bindings e

let eval_fspec eval_symbol line bindings = function
  | FEmpty -> 5
  | FExpr e -> eval_expr eval_symbol line bindings e

let parse_loc t =
  let end_loc_found = ref false in
  List.fold_left (fun (nbline, bindings) line ->
      if !end_loc_found then failwith "Il ne peut pas y avoir d'instruction après END"
      else
        match line with
        | SymDefInstr (sym, AssInstr i) when i.op = EQU ->
          let word = Word.empty () in
          eval_wval eval_symbol_loc word 0 bindings i.addr;
          (nbline, bind_symbol sym (Word.to_int word) bindings)
        | SymDefInstr (sym, AssInstr i) when i.op = ORIG ->
          let word = Word.empty () in
          eval_wval eval_symbol_loc word 0 bindings i.addr;
          let addr = Word.to_int word in
          (addr, bind_symbol sym nbline bindings)
        | SymDefInstr (sym, _) ->
          (nbline + 1, bind_symbol sym nbline bindings)
        | Instr (AssInstr i) when i.op = EQU -> (nbline, bindings)
        | Instr (AssInstr i) when i.op = ORIG ->
          let word = Word.empty () in
          eval_wval eval_symbol_loc word 0 bindings i.addr;
          (Word.to_int word, bindings)
        | Instr _ ->
          (nbline + 1, bindings)
    ) (0, StringMap.empty) t

let parse_literals t end_loc =
  let _, _, t = List.fold_left (fun (added_instrs, add_loc, instrs) line ->
      match line with
      | SymDefInstr (sym, MixInstr i) ->
        begin match i.addr with
        | ALiteral w ->
          (Instr (AssInstr {op = CON; addr = w}) :: added_instrs,
           add_loc + 1,
           SymDefInstr (sym, MixInstr {i with addr = AExpr (EPos (ENum add_loc))}) :: instrs)
        | _ ->
          (added_instrs, add_loc, line :: instrs)
        end
      | SymDefInstr (sym, AssInstr i) when i.op = END ->
        (added_instrs, add_loc, line :: (added_instrs @ instrs))
      | SymDefInstr (sym, _) -> (added_instrs, add_loc, line :: instrs)
      | Instr (MixInstr i) ->
        begin match i.addr with
        | ALiteral w ->
          (Instr (AssInstr {op = CON; addr = w}) :: added_instrs,
           add_loc + 1,
           Instr (MixInstr {i with addr = AExpr (EPos (ENum add_loc))}) :: instrs)
        | _ ->
          (added_instrs, add_loc, line :: instrs)
        end
      | Instr (AssInstr i) when i.op = END ->
        (added_instrs, add_loc, line :: (added_instrs @ instrs))
      | Instr _ -> (added_instrs, add_loc, line :: instrs)
    ) ([], end_loc, []) t in List.rev t

let add_counters bindings =
  StringMap.fold (fun k v b -> StringMap.add k (0, v) b) bindings StringMap.empty

let incr_counter bindings sym =
  let s = match sym with
    | SymString s -> s
    | SymLocal i  -> string_of_int i ^ "h" in
  try
    let c, l = StringMap.find s bindings in
    StringMap.add s (c + 1, l) bindings
  with Not_found ->
    (* ne peut pas arriver car tous les symboles ont déjà été ajoutés aux
       bindings *)
    assert (false)

let eval_mixinstr eval_symbol line bindings (i : mix_instr) =
  let addr = AExpr (EPos (ENum (eval_addr eval_symbol line bindings i.addr))) in
  let index = IExpr (EPos (ENum (eval_index eval_symbol line bindings i.index))) in
  let fspec = match i.fspec with
    | FEmpty -> FEmpty
    | _ -> FExpr (EPos (ENum (eval_fspec eval_symbol line bindings i.fspec))) in
  {i with addr = addr; index = index; fspec = fspec}

let eval_assinstr eval_symbol line bindings (i : ass_instr) =
  let word = Word.empty () in
  eval_wval eval_symbol word line bindings i.addr;
  let wval = WAtomic (WExpr (EPos (ENum (Word.to_int word)), FEmpty)) in
  {i with addr = wval}

let inline_symbols bindings t =
  let (_, t, _) = List.fold_left (fun (loccounter, instrs, bindings) line ->
      match line with
      | SymDefInstr (sym, MixInstr i) ->
        (loccounter + 1,
         Instr (MixInstr (eval_mixinstr eval_symbol_code 0 bindings i)) :: instrs,
         incr_counter bindings sym)
      | SymDefInstr (sym, AssInstr i) ->
        if i.op = EQU then
          (loccounter + 1,
           Instr (AssInstr ({op = CON; addr = WAtomic (WExpr (EPos (ENum 0), FEmpty))})) :: instrs,
           incr_counter bindings sym)
        else
          (loccounter + 1,
           Instr (AssInstr (eval_assinstr eval_symbol_code 0 bindings i)) :: instrs,
           incr_counter bindings sym)
      | SymDefInstr (sym, AlfInstr i) ->
        (loccounter + 1,
         Instr (AlfInstr i) :: instrs,
         incr_counter bindings sym)
      | Instr (MixInstr i) ->
        (loccounter + 1,
         Instr (MixInstr (eval_mixinstr eval_symbol_code 0 bindings i)) :: instrs,
         bindings)
      | Instr (AssInstr i) ->
        if i.op = EQU then
          (loccounter + 1,
           Instr (AssInstr ({op = CON; addr = WAtomic (WExpr (EPos (ENum 0), FEmpty))})) :: instrs,
           bindings)
        else
          (loccounter + 1,
           Instr (AssInstr (eval_assinstr eval_symbol_code 0 bindings i)) :: instrs,
           bindings)
      | Instr (AlfInstr i) ->
        (loccounter + 1,
         Instr (AlfInstr i) :: instrs,
         bindings)
    ) (0, [], add_counters bindings) t in List.rev t

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
      let end_loc = nbline - 1 in
      printf "\nEND LOCATION : %d@." end_loc;
      printf "\nBINDINGS :\n%a@." pp_bindings bindings;
      let inlined_literals = parse_literals t end_loc in
      printf "\nAST INLINED LITERALS :\n%a@." pp_ast inlined_literals;
      let inlined_t = inline_symbols bindings inlined_literals in
      printf "\nAST INLINED :\n%a@." pp_ast inlined_t;
      let mem = empty_memory () in
      let start_loc = inlined_ast_to_memory mem inlined_t in
      printf "MEMORY AFTER LOADING PROGRAM :\n%a@." pp_memory mem;
      printf "STARTING LOCATION : %d@." start_loc
    with _ ->
      let p = lb.lex_curr_p in
      Format.printf "Error : line %d, column %d" p.pos_lnum (p.pos_cnum - p.pos_bol)
  end else
    failwith "Argument non fourni"
