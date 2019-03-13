module Src = SourceLocalisedAST
module Imp = ImpAST
open CommonAST

let assoc_index k l =
  let rec search i = function
    | [] -> raise Not_found
    | (a,_)::l when a=k -> i
    | c::l -> search (i+1) l
  in
  search 0 l


let rec strip_expression type_context e = match Src.(e.expr) with
  | Src.Literal lit ->
    Imp.Literal lit
      
  | Src.Location loc ->
    Imp.Location (strip_location type_context loc)
      
  | Src.UnaryOp(op, e) ->
    Imp.UnaryOp(op, strip_expression type_context e)
      
  | Src.BinaryOp(op, e1, e2) ->
    Imp.BinaryOp(op, strip_expression type_context e1, strip_expression type_context e2)

  | Src.NewArray(e, _) ->
    Imp.NewBlock(strip_expression type_context e)

  | Src.NewRecord(name) ->
    let struct_type = Symb_Tbl.find name type_context.struct_types in
    let size = List.length struct_type.fields in
    Imp.NewBlock(Imp.Literal(Int(size)))

  (* Appel de fonction : rien de spécial, on traduit chaque paramètre. *)
  | Src.FunCall(id, params) ->
    Imp.FunCall(id, List.map (strip_expression type_context) params)

      
and strip_location type_context = function
  | Src.Identifier id ->
    Imp.Identifier id

  | Src.ArrayAccess(e1, e2) ->
    Imp.BlockAccess(strip_expression type_context e1,
                    strip_expression type_context e2)

  | Src.FieldAccess(e, field_name) ->
    let e_type = SourceTypeChecker.type_expression type_context e in
    let struct_name = match e_type with
      | TypStruct(name) -> name
      | _ -> raise (SourceTypeChecker.Struct_type_expected(e_type, Src.(e.e_pos)))
    in
    let fields = (Symb_Tbl.find struct_name type_context.struct_types).fields in
    let index = assoc_index field_name fields in
    Imp.BlockAccess(strip_expression type_context e, Imp.Literal(Int index))

      
let rec strip_instruction type_context i = match Src.(i.instr) with
  | Src.Print e -> 
    Imp.Print (strip_expression type_context e)
      
  | Src.Set(loc, e) ->
    Imp.Set(strip_location type_context loc, strip_expression type_context e)
      
  | Src.Conditional(e, i1, i2) ->
    Imp.Conditional(strip_expression type_context e,
                    strip_instruction type_context i1,
                    strip_instruction type_context i2)

  | Src.Loop(e, i) ->
    Imp.Loop(strip_expression type_context e, strip_instruction type_context i)
      
  | Src.Sequence(i1, i2) ->
    Imp.Sequence(strip_instruction type_context i1,
                 strip_instruction type_context i2)
      
  | Src.Nop ->
    Imp.Nop

  (* Instruction de retour : rien de spécial, on traduit le paramètre. *)
  | Src.Return e ->
    Imp.Return(strip_expression type_context e)

      
let strip_program p =
  let type_context = SourceTypeChecker.extract_context p in
  let main = strip_instruction type_context Src.(p.main) in
  let globals = Src.(p.globals) in
  (* Traduction des informations des fonctions. *)
  (* Pour chaque fonction dans la table... *)
  let functions = Symb_Tbl.map (fun f_info ->
    (* récupérer le contexte de typage étendu... *)
    let context = SourceTypeChecker.extend_context type_context f_info in
    (* traduire le corps de la fonction... *)
    let code = strip_instruction context Src.(f_info.code) in
    (* et construire la définition de fonction modifiée. *)
    Imp.({ signature = Src.(f_info.signature);
           locals = Src.(f_info.locals);
           code })
  ) Src.(p.functions)
  in
  Imp.({ main; globals; functions; })

