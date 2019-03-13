open CommonAST
open GotoAST
open Mips

let push reg = sw reg 0 sp  @@ subi sp sp 4
let pop  reg = addi sp sp 4 @@ lw reg 0 sp

(* Partie préliminaire dédiée à la gestion des paramètres et des variables
   locales, qu'on appellera "identifiants locaux" d'une fonction. *)

(* Pour chaque identifiant local on veut connaître sa nature (paramètre formel
   ou variable locale) et son numéro. *)
type allocation_info =
  (* Cas 1 : paramètre formel. *)
  | Param of int (* Le numéro correspond à l'ordre dans la signature. *)
  (* Cas 2 : variable locale *)
  | Local of int (* Le numéro correspond à l'ordre dans les déclarations. *)

(* Lors de la traduction d'une fonction, on garde sous la main la table de
   ses identifiants locaux, ainsi que le nombre de variables locales. *)
type allocation_context = {
  allocation: allocation_info Symb_Tbl.t;
  nb_locals: int
}

(* Contexte vide : ni paramètre formel ni variable locale.  *)
let empty_allocation_context = { allocation = Symb_Tbl.empty;
                                 nb_locals = 0; }

(* Extraction du contexte d'allocation à partir d'une définition de fonction. *)
let mk_allocation_context function_info =
  (* Construction d'une table d'identifiants locaux contenant les paramètres
     formels. Le numéro correspond à l'ordre dans la signature, en comptant à
     partir de 0. *)
  let allocation = List.fold_left
    (let count = ref (-1) in
     fun alloc (id, _) -> Symb_Tbl.add id (incr count; Param !count) alloc)
    Symb_Tbl.empty
    function_info.signature.formals
  in
  (* Ajout à la table des variables locales. Le numéro correspond à l'ordre dans
     la séquence de déclarations, en comptant à partir de 0. *)
  let allocation = Symb_Tbl.fold
    (let count = ref (-1) in
     fun id _ alloc -> Symb_Tbl.add id (incr count; Local !count) alloc)
    function_info.locals
    allocation
  in
  (* Nombre de variables locales. *)
  let nb_locals = Symb_Tbl.cardinal function_info.locals in
  (* Contexte d'allocation formé avec les éléments précédents. *)
  { allocation; nb_locals }


let translate_literal = function
  | Int i  -> li t0 i
  | Bool b -> li t0 (if b then -1 else 0)

(* Chargement de l'adresse d'un emplacement dans le registre $t0.
   Cette fonction de traduction (et par extension toutes les autres) doit
   recevoir en paramètre supplémentaire le contexte d'allocation qui permet
   de connaître la manière d'accéder à chaque variable. *)
let rec translate_location alloc_context = function
  (* Cas d'un identifiant. On va devoir distinguer entre trois natures :
     paramètre formel de fonction, variable locale, variable globale. *)
  | Identifier (Id id) ->
    begin
      try
        (* Consultation du contexte d'allocation, qui contient les paramètres
           formels et les variables locales. *)
        match Symb_Tbl.find id alloc_context.allocation with
          (* Cas du paramètre formel de numéro [n] : l'adresse est obtenue par
             un décalage positif de 4(n+1) à partir de $fp. *)
          | Param n -> addi t0 fp (4*(n+1))
          (* Cas de la variable locale de numéro [n] : l'adresse est obtenue par
             un décalage négatif de 4(n+2) à partir de $fp. *)
          | Local n -> subi t0 fp (4*(n+2))
      with
        (* Si l'identifiant n'est pas dans la table d'allocation, alors il
           s'agit d'une variable globale, on reprend la méthode d'accès
           précédente. *)
        | Not_found -> la t0 id
    end

  | BlockAccess (a, i) ->
    translate_expression alloc_context i
    @@ pop t0
    @@ li t1 4      
    @@ mul t0 t0 t1
    @@ push t0
    @@ translate_expression alloc_context a
    @@ pop t0 
    @@ pop t1 
    @@ add t0 t0 t1


(* Calcul de la valeur d'une expression. La valeur résultat est placée
   au sommet de la pile. *)
and translate_expression alloc_context = function
  | Literal lit ->
    translate_literal lit
    @@ push t0           
    
  | Location loc ->
    translate_location alloc_context loc (* Chargement de l'adresse dans t0 *)
    @@ lw t0 0 t0                        (* Lecture de la valeur            *)
    @@ push t0                           (* Stockage sur la pile            *)
    
  | UnaryOp(uop, e) ->
    let op = match uop with  
      | Minus -> neg
      | Not -> not_
    in
    translate_expression alloc_context e 
    @@ pop t0              
    @@ op t0 t0            
    @@ push t0             
      
  | BinaryOp(bop, e1, e2) ->
    let op = match bop with
      | Add  -> add
      | Sub  -> sub
      | Mult -> mul
      | Div  -> div
      | Mod  -> rem
      | Eq   -> seq
      | Neq  -> sne
      | Lt   -> slt
      | Le   -> sle
      | Gt   -> sgt
      | Ge   -> sge
      | And  -> and_
      | Or   -> or_
    in
    translate_expression alloc_context e2   
    @@ translate_expression alloc_context e1
    @@ pop t0                 
    @@ pop t1
    @@ op t0 t0 t1            
    @@ push t0                

  | NewBlock(e) ->
    translate_expression alloc_context e
    @@ pop t0
    @@ push t0
    @@ addi t0 t0 1 
    @@ li t1 4
    @@ mul a0 t0 t1
    @@ li v0 9
    @@ syscall
    @@ pop t0
    @@ sw t0 0 v0
    @@ addi t0 v0 4 
    @@ push t0

  (* Cas de l'appel de fonction.
     On suit ici la partie "appelant" du protocole d'appel.  *)
  | FunCall(Id id, params) ->
    (* Étape 2 -> évaluation des paramètres effectifs du dernier au premier,
       dont les valeurs sont placées sur la pile. *)
    let params_code =
      List.fold_right
        (fun e code -> code @@ translate_expression alloc_context e)
        params
        nop
    in
    params_code
    (* Étape 3 -> appel. *)
    @@ jal id
    (* L'appelant n'a pas la main pendant l'étape 4. *)
    (* Étape 5 -> désallocation de l'espace utilisé pour passer les valeurs des
       paramètres effectifs. *)
    @@ pop t0                      (* Sauvegarde du résultat *)
    @@ addi sp sp (4 * List.length params)  (* Désallocation *)
    @@ push t0                   (* Restauration du résultat *)
      
      
let rec translate_instruction alloc_context = function
  | Sequence(i1, i2) ->
    translate_instruction alloc_context i1   
    @@ translate_instruction alloc_context i2
      
  | Print(e) ->
    translate_expression alloc_context e 
    @@ pop a0              
    @@ li v0 11
    @@ syscall
      
  | Set(loc, e) ->
    translate_expression alloc_context e 
    @@ translate_location alloc_context loc
    @@ pop t1           
    @@ sw t1 0 t0       
      
  | Label(Lab lab) -> 
    label lab      
      
  | Goto(Lab lab) ->  
    b lab          
      
  | ConditionalGoto(Lab lab, e) ->
    translate_expression alloc_context e
    @@ pop t0             
    @@ bnez t0 lab        
      
  | Nop -> nop

  (* Instruction de retour.
     Ici on suit la fin de la partie "appelé" du protocole d'appel. *)
  | Return(e) ->
    (* Évaluation de l'expression résultat et stockage sur la pile. *)
    translate_expression alloc_context e
    (* Section ajoutée entre les étapes 3 et 4 (sujets C et 4.2) ->
       désallocation de la pile et restauration des valeurs précédentes des
       registres $ra et $fp. *)
    @@ pop t0        (* Récupération du résultat *)
    @@ move sp fp    (* Désallocation de la pile *)
    @@ lw ra (-4) fp (* Récupération de l'adresse de retour *)
    @@ lw fp 0 fp    (* Restauration du pointeur de base de l'appelant *)
    @@ push t0       (* Le résultat est replacé sur la pile *)
    (* Étape 4 -> retour *)
    @@ jr ra

      
(* Traduction du code d'une fonction définie par l'utilisateur.
   Ici on suit le début de la partie "appelé" du protocole d'appel. *)
let translate_function function_info =
  (* Extraction du contexte d'allocation. *)
  let alloc_context = mk_allocation_context function_info in
  (* Étape 2 (et éléments ajoutés entre 1 et 2 dans le sujet C) -> sauvegarde
     des valeurs courantes des registres $fp et $ra, définition de la nouvelle
     valeurs de $fp. *)
  push fp         (* Sauvegarde de $fp *)
  @@ push ra      (* Sauvegarde de $ra *)
  @@ addi fp sp 8 (* Définition de $sp *)
  (* Entre les étapes 2 et 3 (sujet 4.2) -> allocation de l'espace destiné aux
     variables locales *)
  @@ addi sp sp (-8 * alloc_context.nb_locals)
  (* Étape 3 -> exécution du code du corps de la fonction. *)
  @@ translate_instruction alloc_context function_info.code
  (* Normalement, la fin du protocole sera gérée par l'instruction [return]. *)
    
  (* Il est possible d'ajouter ici du code de rattrapage pour le cas où 
     l'exécution du code de la fonction n'a pas rencontrée de [return], ce
     qui serait un bug imputable au programmeur. *)
  (* Alternative : on peut aussi placer ici le code gérant la fin du protocole
     d'appel, et remplacer le code de [return] actuel par un saut à cet
     épilogue. *)

      
let translate_program program =
  let init =
    beqz a0 "init_end"
    @@ lw a0 0 a1
    @@ jal "atoi"
    @@ la t0 "arg"
    @@ sw v0 0 t0
    @@ label "init_end"
      
  and close =
    li v0 10
    @@ syscall

  and built_ins =
    label "atoi"      
    @@ move t0 a0 
    @@ li   t1 0  
    @@ li   t3 10 
    @@ li   t4 48 
    @@ li   t5 57 
    @@ label "atoi_loop"
    @@ lbu  t2 0 t0 
    @@ beq  t2 zero "atoi_end" 
    @@ blt  t2 t4 "atoi_error" 
    @@ bgt  t2 t5 "atoi_error"
    @@ addi t2 t2 (-48) 
    @@ mul  t1 t1 t3
    @@ add  t1 t1 t2 
    @@ addi t0 t0 1
    @@ b "atoi_loop"
    @@ label "atoi_error"
    @@ li   v0 10
    @@ syscall
    @@ label "atoi_end"
    @@ move v0 t1
    @@ jr   ra
      
    @@ comment "print_int"
    @@ label "print_int"
    @@ lw a0 4 sp
    @@ li v0 1
    @@ syscall
    @@ sw a0 0 sp
    @@ subi sp sp 4
    @@ jr ra
      
    @@ comment "power"
    @@ label "power"
    @@ lw s0 8 sp
    @@ lw s1 4 sp
    @@ li t0 1
    @@ b "power_loop_guard"
    @@ label "power_loop_code"
    @@ mul t0 t0 s1
    @@ subi s0 s0 1
    @@ label "power_loop_guard"
    @@ bgtz s0 "power_loop_code"
    @@ sw t0 0 sp
    @@ subi sp sp 4
    @@ jr ra
  in

  (* Traduction du bloc de code principal dans le context vide. *)
  let main_code = translate_instruction empty_allocation_context program.main in
  (* Traduction du code des fonctions définies par l'utilisateur. *)
  let function_codes = Symb_Tbl.fold
    (fun id function_info code ->
      (* Chaque code de fonction est précédé d'une étiquette (donnée par son
         identifiant) permettant les sauts. *)
      label id @@ translate_function function_info @@ code)
    program.functions nop
  in
  (* Le code des fonctions est ajouté à la séquence principale. *)
  let text = init @@ main_code @@ close @@ function_codes @@ built_ins in

  let data = Symb_Tbl.fold
    (fun var _ code -> label var @@ dword [0] @@ code)
    program.globals nop
  in

  { text; data }
