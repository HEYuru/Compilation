%{

  open Lexing
  open CommonAST
  open SourceLocalisedAST
  
%}

%token <int> CONST_INT
%token <bool> CONST_BOOL
%token <string> IDENT
%token PLUS MINUS STAR DIV MOD
%token EQUAL NEQ LE LT GE GT
%token AND OR NOT
%token LP RP
(* Nouveaux lexèmes *)
%token COMMA
%token RETURN

%token VAR
%token INTEGER BOOLEAN

%token MAIN
%token IF ELSE WHILE
%token SEMI
%token SET PRINT
%token BEGIN END
%token EOF

%token LB RB NEW
%token DOT STRUCT

%left SEMI
%left AND OR
%left GE GT LE LT EQUAL NEQ
%left PLUS MINUS
%left STAR DIV
%left MOD
%nonassoc NOT
%nonassoc NEW
%nonassoc LB
%left DOT

%start prog
%type <SourceLocalisedAST.program> prog

%%

prog:
(* Règle mise à jour : un programme est formé d'une séquence de déclarations
   de structure, d'une séquence de déclarations de variables et d'une séquence
   de déclarations de fonctions suivies d'un bloc de code principal. *)
| structs=struct_decls; globals=var_decls; functions=fun_decls; main=main; EOF
  { let globals = Symb_Tbl.add "arg" TypInt globals in
    { main; globals; structs; functions } }

| error { let pos = $startpos in
          let message =
            Printf.sprintf "Syntax error at %d, %d" pos.pos_lnum pos.pos_cnum
          in
          failwith message }
;

struct_decls:
| (* empty *)                      { Symb_Tbl.empty }
| sd=struct_decl; sds=struct_decls { let id, s_info = sd in
                                     Symb_Tbl.add id s_info sds }
;

struct_decl:
| STRUCT; id=IDENT; BEGIN; f=fields_decl; END { id, { fields = f } }
;

fields_decl:
| (* empty *)                          { []       }
| tid=typed_ident; SEMI; f=fields_decl { tid :: f }
;

var_decls:
| (* empty *)                      { Symb_Tbl.empty }
| vd=var_decl; SEMI; vds=var_decls { let id, ty = vd in
                                     Symb_Tbl.add id ty vds }
;

var_decl:
| VAR; tid=typed_ident { tid }
;

typed_ident:
| ty=typ; id=IDENT  { id, ty }
;

typ:
| INTEGER        { TypInt       }
| BOOLEAN        { TypBool      }
| ty=typ; LB; RB { TypArray ty  }
| id=IDENT       { TypStruct id }
;

(* Règle pour une séquence de déclarations de fonctions. Renvoie une table des
   symboles associant à chaque identifiant de fonction ses informations de
   définition.
   Type du résultat :
     function_info Symb_Tbl.t
*)
fun_decls:
| (* empty *)                { Symb_Tbl.empty             }
| fd=fun_decl; fds=fun_decls { let id, f_info = fd in
                               Symb_Tbl.add id f_info fds }
;

(* Règle pour une déclaration de fonction. On en extrait l'identifiant de la
   fonction, et une description de type [function_info] elle-même constituée
   d'une signature, d'une table des variables locales et du code.
   Le type du résultat est donc :
     string * function_info
*)
fun_decl:
| return=typ; id=IDENT; LP; formals=separated_list(COMMA, typed_ident); RP;
  BEGIN; locals=var_decls; code=localised_instruction; END
    { let signature = { return; formals; } in
      let info = { signature; locals; code; } in
      id, info }
;

main:
| MAIN; i=block { i }
;

block:
| BEGIN; i=localised_instruction; END { i }
;

localised_instruction:
| i=instruction { let l = $startpos.pos_lnum in
                  let c = $startpos.pos_cnum - $startpos.pos_bol in
                  mk_instr i l c }
;

instruction:
| (* empty *)                                         { Nop             }
| i1=localised_instruction; SEMI; i2=localised_instruction
                                                      { Sequence(i1,i2) }
| PRINT; LP; e=localised_expression; RP               { Print(e)        }
| loc=location; SET; e=localised_expression           { Set(loc, e)     }
| IF; LP; e=localised_expression; RP; i1=block; ELSE; i2=block
                                               { Conditional(e, i1, i2) }
| WHILE; LP; e=localised_expression; RP; i=block      { Loop(e, i)      }
(* Instruction de retour. *)
| RETURN; LP; e=localised_expression; RP              { Return(e)       }
;

localised_expression:
| e=expression { let l = $startpos.pos_lnum in
                 let c = $startpos.pos_cnum - $startpos.pos_bol in
                 mk_expr e l c }
;

expression:
| lit=literal                                  { Literal(lit)          }
| loc=location                                 { Location(loc)         }
| LP; e=expression; RP                         { e                     }
| uop=unop; e=localised_expression             { UnaryOp(uop, e)       }
| e1=localised_expression; bop=binop; e2=localised_expression
                                               { BinaryOp(bop, e1, e2) }
| NEW; ty=typ; LB; e=localised_expression; RB  { NewArray(e, ty)       }
| NEW; ty=typ { match ty with TypStruct(id) -> NewRecord(id) | _ -> assert false }
(* Règle pour l'appel de fonction *)
| id=IDENT; LP; params=separated_list(COMMA, localised_expression); RP
                                               { FunCall(Id id, params) }
;

literal:
| i=CONST_INT   { Int i  }
| b=CONST_BOOL  { Bool b }
;

location:
| id=IDENT                                   { Identifier (Id id)  }
| e1=localised_expression; LB; e2=localised_expression; RB
                                             { ArrayAccess(e1, e2) }
| e=localised_expression; DOT; id=IDENT      { FieldAccess(e, id)  }
;

%inline unop:
| MINUS { Minus }
| NOT   { Not   }
;

%inline binop:
| PLUS   { Add   }
| MINUS  { Sub   }
| STAR   { Mult  }
| DIV    { Div   }
| MOD    { Mod   }
| EQUAL  { Eq    }
| NEQ    { Neq   }
| LT     { Lt    }
| LE     { Le    }
| GT     { Gt    }
| GE     { Ge    }
| AND    { And   }
| OR     { Or    }
;
