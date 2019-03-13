open CommonAST

type localised_expression = {
  expr : expression;
  e_pos : int * int;
}

and expression =
  | Literal  of literal
  | Location of location
  | UnaryOp  of unaryOp  * localised_expression
  | BinaryOp of binaryOp * localised_expression * localised_expression
  | NewArray of localised_expression * typ
  | NewRecord of string
  (* Expression pour l'appel de fonction. *)
  | FunCall  of identifier * localised_expression list
      
and location =
  | Identifier  of identifier
  | ArrayAccess of localised_expression * localised_expression
  | FieldAccess of localised_expression * string

let mk_expr expr l c = { expr = expr; e_pos = l, c }

type localised_instruction = {
  instr : instruction;
  i_pos : int * int;
}

and instruction =
  | Print       of localised_expression
  | Set         of location * localised_expression
  | Conditional of localised_expression * localised_instruction
                                        * localised_instruction
  | Loop        of localised_expression * localised_instruction
  | Sequence    of localised_instruction * localised_instruction
  | Nop
  | Return      of localised_expression

let mk_instr instr l c = { instr = instr; i_pos = l, c }

(* Informations relevées pour chaque fonction, qui seront placées dans la
   table des symboles correspondante. *)
type function_info = {
  (* Signature *)
  signature: function_signature;
  (* Table des variables locales et de leurs types *)
  locals: typ Symb_Tbl.t;
  (* Code *)
  code: localised_instruction;
}
  
type program = {
  main: localised_instruction;
  globals: typ Symb_Tbl.t;
  structs: struct_type Symb_Tbl.t;
  (* Une table des symboles supplémentaire, pour les fonctions. *)
  functions: function_info Symb_Tbl.t;
}
