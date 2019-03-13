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
  | Print       of expression
  | Set         of location   * expression
  | Conditional of expression * instruction * instruction
  | Loop        of expression * instruction
  | Sequence    of instruction * instruction
  | Nop
  (* Instruction de retour. *)
  | Return      of expression

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
