module Imp = ImpAST
module Gto = GotoAST
open CommonAST

let (++) = Gto.(++)

let rec translate_expression = function
  | Imp.Literal lit ->
    Gto.Literal lit

  | Imp.Location loc ->
    Gto.Location(translate_location loc)

  | Imp.UnaryOp(op, e) ->
    Gto.UnaryOp(op, translate_expression e)

  | Imp.NewBlock(e) ->
    Gto.NewBlock(translate_expression e)

  (* Appel de fonction : rien de spécial. *)
  | Imp.FunCall(id, params) ->
    Gto.FunCall(id, List.map translate_expression params)

  | Imp.BinaryOp(op, e1, e2) ->
   begin
    match e1, e2 with
    | Imp.Literal(Int(i1)), Imp.Literal(Int(i2)) -> 
      begin
        match op with
        | Add -> Gto.Literal(Int(i1 + i2))
        | Sub -> Gto.Literal(Int(i1 - i2))
        | Mult -> Gto.Literal(Int(i1 * i2))
        | Div -> Gto.Literal(Int(i1 / i2))
        | Mod -> Gto.Literal(Int(i1 mod i2))
        | Eq -> Gto.Literal(Bool(i1 = i2))
        | Neq -> Gto.Literal(Bool(i1 != i2))
        | Lt -> Gto.Literal(Bool(i1 < i2))
        | Le -> Gto.Literal(Bool(i1 <= i2))
        | Gt -> Gto.Literal(Bool(i1 > i2))
        | Ge -> Gto.Literal(Bool(i1 >= i2))
        | _ -> failwith "OP Error"
      end
   
    | Imp.Literal(Bool(i1)), Imp.Literal(Bool(i2)) -> 
      begin
        match op with
        | And -> Gto.Literal(Bool(i1 && i2))
        | Or -> Gto.Literal(Bool(i1 || i2))
        | _ -> failwith "OP Error"
      end
      
    | _, _-> Gto.BinaryOp(op, translate_expression e1, translate_expression e2)
   end


and translate_location = function
  | Imp.Identifier id ->
    Gto.Identifier id

  | Imp.BlockAccess(e1, e2) ->
    Gto.BlockAccess(translate_expression e1, translate_expression e2)


let new_label =
  let cpt = ref 0 in
  fun () -> incr cpt; CommonAST.Lab (Printf.sprintf "_label_%i" !cpt)

let rec translate_instruction = function
  | Imp.Sequence(i1, i2) ->
    Gto.Sequence(translate_instruction i1,
                 translate_instruction i2)
      
  | Imp.Print(e) ->
    Gto.Print(translate_expression e)
      
  | Imp.Set(loc, e) ->
    Gto.Set(translate_location loc, translate_expression e)

  | Imp.Conditional(c, i1, i2) ->
    let then_label = new_label()
    and end_label = new_label()
    in
    Gto.ConditionalGoto(then_label, translate_expression c)
    ++ translate_instruction i2
    ++ Gto.Goto end_label
    ++ Gto.Label then_label
    ++ translate_instruction i1
    ++ Gto.Label end_label

  | Imp.Loop(c, i) ->
    let test_label = new_label()
    and code_label = new_label()
    in
    Gto.Goto test_label
    ++ Gto.Label code_label
    ++ translate_instruction i
    ++ Gto.Label test_label
    ++ Gto.ConditionalGoto(code_label, translate_expression c)

  | Imp.Nop ->
    Gto.Nop

  (* Traduction d'une instruction de retour : direct. *)
  | Imp.Return e ->
    Gto.Return(translate_expression e)

(* Traduction de la définition d'une fonction. *)
let translate_function f_info = Gto.({
  (* Signature et tables des variables locales ne changent pas. *)
  signature = Imp.(f_info.signature);
  locals = Imp.(f_info.locals);
  (* Le corps de la fonction est traduit directement. *)
  code = translate_instruction Imp.(f_info.code);
})

      
let translate_program p = Gto.({
  main = translate_instruction Imp.(p.main);
  globals = Imp.(p.globals);
  (* Traduire l'ensemble des fonctions. *)
  functions = Symb_Tbl.map translate_function Imp.(p.functions);
})
