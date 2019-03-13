open CommonAST
open GotoAST
module Iga = IndexedGotoAST

type succ_table = int list array

type liveness_info = { live_in: string list array;
                       live_out: string list array }

let change = ref true

let get_num instr =
	match instr with
  		| (n, Iga.Sequence(i1, i2)) -> n+1
  		| _ -> 1 

let mk_succ_table instr = 
  let array = 
  	match instr with
  	| _ -> Array.make 1 []
  	| (n, Iga.Sequence(i1, i2)) -> Array.make (n+1) []  	 
  in


let rec index_label instr s = 
  	match instr with
    | (n, _) -> 0
  	| (n, Iga.Label(l)) -> if l = s then n else 0  
  	| (n, Iga.Sequence(i1, i2)) -> (index_label i1 s) + (index_label i2 s)
    in


let rec mk_rec s = 
  	match s with
  	| (n, Iga.Goto(l)) -> Array.set array n [(index_label instr l)]	
    | (n, Iga.ConditionalGoto(l,e)) -> 
  	  let one = Array.get array n in
  	  let n_l = (index_label instr l) in
  	  let total = n_l :: one in
  	  Array.set array n total
  	| (n, Iga.Sequence((n1, i1),(n2, i2))) -> 
  	  Array.set array n1 [n2];
  	  Array.set array n2 (Array.get array n);
  	  mk_rec (n1, i1);
  	  mk_rec (n2, i2)
	| _ -> ()
  in

mk_rec instr;

let succ : succ_table = array in
  succ


(*Array subtraction *)
let sub_list l1 l2 = 
  let rec in_list l v = 
  	match l with
  	| [] -> false
 	| h::r -> if h = v then true else in_list r v
  in
  if ((List.length l2 = 0) || (in_list l1 (List.hd l2)) = false)  
  then l1
  else 
    begin
  	  let v = List.hd l2 in
  	  (* sub variable v *)
  	  let rec sub l1 v = 
  		match l1 with
  		| [] -> l1
  		| l::s -> if l = v then s else (l :: (sub s v))
  	in
  	sub l1 v
   end


(* mix two list *)
let combine_list l1 l2 = 
   (*Check the variable is in the list *)
  let rec in_list l v = 
  	match l with
  	| [] -> false
 	| h::r -> if h = v then true else in_list r v
  in
  (*Add variables to list*)
  let add_variable l v = v::l in
  (*mix variables*)
  let rec union_list l1 l2 = 
  	match l2 with
  	| [] -> l1
  	| s::l -> if(in_list l1 s) 
  			  then union_list l1 l 
  			  else union_list (add_variable l1 s) l
    in
    union_list l1 l2
 
(* Compare two lists if they are equal *)
let rec list_equal l1 l2 = 
 	match l1, l2 with
 	| [], [] -> true
 	| [], _ | _, [] -> false
 	| h1::r1, h2::r2 -> if h1=h2 then list_equal r1 r2 else false


let get_name str = 
	let l = String.split_on_char '.' str in
	let rec name l acc = match l with
		| [] -> failwith " error "
		| [v] -> []
		| h::r -> (acc^h) :: (name r (acc^h^"."))
	in
	name l ""


let rec liveness_rec instr successeur kill gen liveness_info = 
  match instr with
  	| (n, Iga.Sequence((n1, i1),(n2, i2))) -> 
  		let live_in = liveness_info.live_in in
  		let live_out = liveness_info.live_out in
  		let live_in1 = Array.get live_in n in
  		let live_out1 = Array.get live_out n in
  		let live_in_g = Array.get live_in n1 in
  		let live_out_d = Array.get live_out n2 in
  		if (list_equal live_in1 live_in_g) = false || (list_equal live_out1 live_out_d) = false
  		then change := true;
  		Array.set live_in n (live_in_g);
  		Array.set live_out n (live_out_d);
  		liveness_rec (n1, i1) successeur kill gen liveness_info;
  		liveness_rec (n2, i2) successeur kill gen liveness_info


  	| (n, _)-> 
  		let live_in = liveness_info.live_in in
  		let live_out = liveness_info.live_out in
  		let in1 = Array.get live_in n in
  		let out1 = Array.get live_out n in
  		let succ1 = Array.get successeur n in
  		let gen1 = Array.get gen n in
  		let kill1 = Array.get kill n in
  		
  		let out2 = 
  			List.fold_right 
  			( fun index_succ b -> 
  				let in_succ = Array.get live_in index_succ in 
  				(combine_list b in_succ) )
  			succ1 
  			out1
  		in 
  		if (list_equal out1 out2) = false then change := true;
  		let in2 = sub_list out2 kill1 in
  		let in2 = combine_list in2 gen1 in
  		if (list_equal in1 in2) = false then change := true;
  		Array.set live_in n in2;
  		Array.set live_out n out2

and get_location l = match l with
  | Identifier (Id id) -> [id]

  | BlockAccess (a, i) ->
  	let Literal(Int(indice)) = i in
  	let parent = get_variable a in
  	[(List.hd parent)^"."^(string_of_int indice)]

and get_variable expr = match expr with
  | Literal lit -> []

  | Location loc -> get_location loc
    
  | UnaryOp(uop, e) -> get_variable e   
      
  | BinaryOp(bop, e1, e2) -> 
  	combine_list (get_variable e1) (get_variable e2)

  | NewBlock(e) -> get_variable e

  | FunCall(Id id, params) -> 
  	let variables = List.fold_right (fun e l -> combine_list l  (get_variable e)) params []
    in variables


and gen_kill instr gen kill = 
  	match instr with
  	| (n, Iga.Sequence((n1, i1),(n2, i2))) -> 
  		gen_kill (n1, i1) gen kill;
  		gen_kill (n2, i2) gen kill
  	| (n, Iga.Print(e)) -> Array.set gen n (get_variable e)
  	| (n, Iga.Set(l, e)) -> 
  		let g = get_variable e in
  		let k = get_location l in
  		let str = List.hd k in
  		if (String.contains str '.') then
  			Array.set gen n (combine_list g (get_name str))
  		else
  			Array.set gen n g;
  		Array.set kill n k
  	| (n, Iga.Goto(l)) -> ()
  	| (n, Iga.ConditionalGoto(l,e)) -> Array.set gen n (get_variable e)
  	| (n, Iga.Nop) -> ()
  	| (n, Iga.Return(e)) -> Array.set gen n (get_variable e)
 	| (n, Iga.Label(l)) -> ()

let rec liveness instr = 
	let n = get_num instr in
	let kill = Array.make n [] in
	let gen = Array.make n [] in
	let succ = mk_succ_table instr in
	gen_kill instr gen kill;
	let info = { live_in = Array.make n [];
	  			 live_out = Array.make n []; }
	in
	change := true;
	while (!change) = true do
		change := false;
		liveness_rec instr succ kill gen info;
	done;
	info
	
  
