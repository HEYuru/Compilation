type identifier = Id  of string
type label      = Lab of string

module Symb_Tbl = Map.Make(String)
    
type typ =
  | TypInt
  | TypBool
  | TypArray of typ
  | TypStruct of string

type struct_type = {
  fields: (string * typ) list;
}

(* Signature d'une fonction : type de retour (obligatoire) et liste des 
   paramètres formels et leurs types. *)
type function_signature = {
  return: typ;
  formals: (string * typ) list;
}
    
type type_context = {
  identifier_types: typ Symb_Tbl.t;
  struct_types: struct_type Symb_Tbl.t;
  (* Table des signatures de fonctions. *)
  function_types: function_signature Symb_Tbl.t;
  (* Type de retour attendu, qui est défini lorsque l'on vérifie le typage du
     corps d'une fonction. *)
  expected_return: typ option
}

type literal =
  | Int  of int
  | Bool of bool

type unaryOp = Minus | Not
    
type binaryOp = Add | Sub | Mult | Div | Mod
                | Eq | Neq | Lt | Le | Gt | Ge
                | And | Or
