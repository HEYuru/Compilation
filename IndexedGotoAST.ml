module Gto = GotoAST
open GotoAST
open CommonAST

type instruction = int * instr_descr

and instr_descr =
  | Sequence        of instruction * instruction
  | Print           of expression
  | Set             of location * expression
  | Label           of label
  | Goto            of label
  | ConditionalGoto of label * expression
  | Return          of expression
  | Nop

type function_info = {
  signature: function_signature;
  locals: typ Symb_Tbl.t;
  code: instruction;
}

type program = {
  main: instruction;
  globals: typ Symb_Tbl.t;
  functions: function_info Symb_Tbl.t;
}

let get_index = ref (
  let cpt = ref 0 in
  fun () -> incr cpt; !cpt )

let mk_instr i = ((!get_index)(), i)
 

let rec index_instruction goto =
  match goto with
  | Gto.Sequence(i1, i2)  
    ->  let i1 = (index_instruction i1) in
        let i2 = (index_instruction i2) in
        mk_instr (Sequence(i1, i2))     
  | Gto.Print(e)     
    -> mk_instr (Print(e))             
  | Gto.Set(l, e)
    -> mk_instr (Set(l, e))
  | Gto.Label(l) 
    -> mk_instr (Label(l))   
  | Gto.Goto(l)
    -> mk_instr (Goto(l))          
  | Gto.ConditionalGoto(l, e)
    -> mk_instr (ConditionalGoto(l, e)) 
  | Gto.Nop
    -> mk_instr (Nop)
  | Gto.Return(e)
    -> mk_instr (Return(e))
 

let rec strip_instruction = function
  | (i, Sequence(i1, i2)) -> 
    Gto.Sequence(strip_instruction i1, strip_instruction i2)
  | (i, Print(e)) -> Gto.Print(e)
  | (i, Set(l, e)) -> Gto.Set(l, e)
  | (i, Goto(e)) -> Gto.Goto(e)
  | (i, ConditionalGoto(l,e)) -> Gto.ConditionalGoto(l, e)
  | (i, Nop) -> Gto.Nop
  | (i, Return(e)) -> Gto.Return(e)
  | (i, Label(e)) -> Gto.Label(e)

let index_function info = 
  get_index := 
  (let cpt = ref 0 in fun () -> incr cpt; !cpt);
  {locals = Gto.(info.locals);
   signature = Gto.(info.signature);
   code = index_instruction Gto.(info.code);}

let index_program p = 
{
  main = index_instruction Gto.(p.main);
  globals = Gto.(p.globals);
  functions = Symb_Tbl.map index_function Gto.(p.functions);
}

let strip_function info =
 {
  Gto.signature = info.signature;
  locals = info.locals;
  code = strip_instruction (info.code);
}

let strip_program p = {
  Gto.main = strip_instruction (p.main);
  globals = p.globals;
  functions = Symb_Tbl.map strip_function p.functions;
}



