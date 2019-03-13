open CommonAST
module Iga = IndexedGotoAST


let list_list l1 l2 = 
	let rec element_list l1 e = 
	 List.fold_left (fun a blist -> if e = blist then true else (false||a) ) false l1 in
	 List.fold_left(fun a blist -> a && (element_list l1 blist) ) true l2 


let rec step_rec instr kill liveness_info = 
	match instr with
	| (n, Iga.Sequence((n1, i1),(n2, i2))) -> 
		let (v1, iv1) = step_rec (n1, i1) kill liveness_info in
		let (v2, iv2) = step_rec (n2, i2) kill liveness_info in
		if (iv1 = true || iv2 = true) 
		then ((n, Iga.Sequence(v1, v2)), true)
		else (instr, false)

	| (e, i) -> 
		let kills = Array.get kill e in
		let live_outs = IndexedGotoLiveness.(liveness_info.live_out) in
		let live_outs = Array.get live_outs e in
		if (list_list live_outs kills) then 
			(instr, false)
		else 
			begin
			Printf.printf " find %d  \n" e;
			((e, Iga.Nop), true)
			end
			

let step instr = 
	let n = 
  		match instr with
  		| (n, Iga.Sequence(i1, i2)) -> n+1
  		| _ -> 1
  	in
	let kill = Array.make n [] in
	let gen = Array.make n [] in
	IndexedGotoLiveness.gen_kill instr gen kill;
	let info : IndexedGotoLiveness.liveness_info = IndexedGotoLiveness.liveness instr in
	step_rec instr kill info


let rec dead_code_elim instr =
	let (instruction, change) = step instr in
	if change 
	then dead_code_elim instruction
	else instruction



