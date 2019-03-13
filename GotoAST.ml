open CommonAST

type expression =
  | Literal  of literal
  | Location of location
  | UnaryOp  of unaryOp  * expression
  | BinaryOp of binaryOp * expression * expression
  | NewBlock of expression
  (* Appel de fonction. *)
  | FunCall  of identifier * expression list

and location =
  | Identifier  of identifier
  | BlockAccess of expression * expression
      
type instruction =
  | Sequence        of instruction * instruction
  | Print           of expression
  | Set             of location * expression
  | Label           of label
  | Goto            of label
  | ConditionalGoto of label * expression
  | Nop
  (* Instruction de retour. *)
  | Return          of expression

let (++) i1 i2 = Sequence(i1, i2)

(* Informations pour les fonctions. *)
type function_info = {
  signature: function_signature;
  locals: typ Symb_Tbl.t;
  code: instruction;
}

type program = {
  main: instruction;
  globals: typ Symb_Tbl.t;
  (* Table des fonctions. *)
  functions: function_info Symb_Tbl.t;
}
