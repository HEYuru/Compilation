open CommonAST
open SourceLocalisedAST

exception Type_error of typ * typ * (int * int)
exception Array_type_expected of typ * (int * int)
exception Struct_type_expected of typ * (int * int)
exception Unexpected_return of (int * int)

let rec check_type context e expected_type =
  let e_type = type_expression context e in
  if e_type = expected_type
  then ()
  else raise (Type_error(e_type, expected_type, e.e_pos))

and type_expression context e = match e.expr with
  | Literal lit -> type_literal lit
  | Location loc -> type_location context loc 

  | UnaryOp(Minus, e) -> check_type context e TypInt; TypInt
  | UnaryOp(Not, e) -> check_type context e TypBool; TypBool
                         
  | BinaryOp(op, e1, e2) ->
    let operand_type, result_type = match op with
      | Add | Sub | Mult | Div | Mod -> TypInt, TypInt
      | Lt | Le | Gt | Ge -> TypInt, TypBool
      | And | Or -> TypBool, TypBool
      | Eq | Neq -> type_expression context e1, TypBool
    in
    check_type context e1 operand_type;
    check_type context e2 operand_type;
    result_type

  | NewArray(e, ty) ->
    check_type context e TypInt;
    TypArray(ty)

  | NewRecord(name) ->
    if Symb_Tbl.mem name context.struct_types
    then TypStruct(name)
    else failwith "Unknown struct"

  (* Typage d'un appel de fonction *)
  | FunCall(Id id, params) ->
    (* Récupération de la signature de la fonction dans la table idoine du
       contexte de typage. *)
    let signature = Symb_Tbl.find id context.function_types in
    (* Vérification du typage de chaque paramètre effectif et comparaison au
       type déclaré dans la signature. *)
    List.iter2 (check_type context) params (List.map snd signature.formals);
    (* Remarque : la ligne précédente déclenche une exception si le nombre des
       paramètres effectifs ne correspond pas au nombre des paramètres formels
       dans la signature. On pourrait rattraper cette exception pour produire un
       message d'erreur circonstancié. *)
    (* Type de l'expression : le type de retour donné par la signature. *)
    signature.return

      
and type_literal = function
  | Int _ -> TypInt
  | Bool _ -> TypBool

and type_location context = function
  | Identifier(Id id) -> Symb_Tbl.find id context.identifier_types

  | ArrayAccess(e1, e2) ->
    let e1_type = type_expression context e1 in
    let contents_type = match e1_type with
      | TypArray t -> t
      | _ -> raise (Array_type_expected(e1_type, e1.e_pos))
    in
    check_type context e2 TypInt;
    contents_type

  | FieldAccess(e, field_name) ->
    let e_type = type_expression context e in
    let fields = match e_type with
      | TypStruct(name) -> (Symb_Tbl.find name context.struct_types).fields
      | _ -> raise (Struct_type_expected(e_type, e.e_pos))
    in
    List.assoc field_name fields

let rec typecheck_instruction context i = match i.instr with
  | Print e -> check_type context e TypInt

  | Set(loc, e) ->
    let loc_type = type_location context loc in
    check_type context e loc_type

  | Conditional(e, i1, i2) ->
    check_type context e TypBool;
    typecheck_instruction context i1;
    typecheck_instruction context i2

  | Loop(e, i) ->
    check_type context e TypBool;
    typecheck_instruction context i
    
  | Sequence(i1, i2) ->
    typecheck_instruction context i1;
    typecheck_instruction context i2

  | Nop -> ()

  (* Vérification du type de l'instruction [return]. *)
  | Return(e) ->
    (* Extraction de l'éventuel type de retour renseigné dans le contexte de
       typage. *)
    let return_type = match context.expected_return with
      | Some typ -> typ
      (* Si ce type n'est pas défini -ce qui signifie que l'on est pas dans le
         corps d'une fonction- alors l'instruction est rejetée. *)
      | None -> raise (Unexpected_return i.i_pos)
    in
    (* Vérification du paramètre et comparaison avec le type de retour. *)
    check_type context e return_type

(**
   L'avez-vous remarqué ?
   ----------------------

   En l'état actuel on ne vérifie pas que toute fonction exécute bien une
   instruction [return] avant d'épuiser son code. D'où quelques questions :
   - Que se passe-t-il si une fonction exécute tout son code sans rencontrer
     d'instruction [return] ?
   - Que faire pour que l'absence de [return] ne résulte pas (ou disons,
     moins systématiquement) en un crash ?
   - Étant donnée une fonction dont le corps contient au moins une instruction
     [return], est-il possible de prédire si l'une de ces instructions sera bien
     exécutée ?
   - Donner un critère qui, s'il est vérifié par le corps de la fonction,
     garanti que l'exécution de cette fonction effectuera bien un [return].
*)

let extract_context p =
  (* Définition des signatures des fonctions prédéfinies. *)
  let print_int_sig = { return=TypInt; formals=["x", TypInt] } in
  let power_sig = { return=TypInt; formals=["x", TypInt; "n", TypInt] } in
  (* Extraction des signatures de chaque fonction, et ajout des signatures
     des fonctions prédéfinies. *)
  let fs = Symb_Tbl.map (fun f_info -> f_info.signature) p.functions in
  let fs = Symb_Tbl.add "print_int" print_int_sig fs in
  let fs = Symb_Tbl.add "power" power_sig fs in
  { identifier_types = p.globals;
    struct_types = p.structs;
    (* Table des signatures de fonctions. *)
    function_types = fs;
    (* Type de retour : indéfini par défaut. *)
    expected_return = None;
  }

(* Vérification du typage d'une fonction. *)
let rec typecheck_function context function_info =
  (* Extension du contexte de typage avec les informations utiles dans le corps
     de la fonction (types des paramètres formels, types des variables locales,
     type de retour). *)
  let context = extend_context context function_info in
  (* Vérification du typage du corps de la fonction dans ce contexte étendu. *)
  typecheck_instruction context function_info.code

(* Extension du contexte du typage pour l'analyse du corps d'une fonction. *)
and extend_context context function_info =
  (* Récupération de la table de symbole des identifiants. *)
  let identifier_types = context.identifier_types in
  (* Ajout des types des paramètres formels. *)
  let identifier_types =
    List.fold_right (fun (id, typ) tbl -> Symb_Tbl.add id typ tbl)
      function_info.signature.formals identifier_types
  in
  (* Ajoute des types des variables locales. *)
  let identifier_types =
    Symb_Tbl.fold (fun id typ tbl -> Symb_Tbl.add id typ tbl)
      function_info.locals identifier_types
  in
  (* Résultat : un nouveau context de typage avec table des types des variables
     et type de retour modifiés. *)
  { identifier_types = identifier_types;
        struct_types = context.struct_types;
      function_types = context.function_types;
     expected_return = Some function_info.signature.return; }

(* Remarque Caml : on pouvait condenser le résultat avec la forme suivante qui
   décrit un élément identique à [context] excepté pour les champs mentionnés.
  
  { context with 
    identifier_types = identifier_types;
     expected_return = Some function_info.signature.return; }

   Une convention syntaxique supplémentaire permet même de réduire encore en
   omettant une valeur qui serait désignée par le même nom que le champ auquel
   elle est affectée. 

  { context with identifier_types;
    expected_return = Some function_info.signature.return; }
*)

let typecheck_program p =
  let type_context = extract_context p in
  typecheck_instruction type_context p.main;
  (* Vérification du typage de chaque fonction. *)
  Symb_Tbl.iter (fun _ -> typecheck_function type_context) p.functions

