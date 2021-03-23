module F = Format
let split lst  =

	let rec split_result lst l1 l2 = 
		match lst with
		| p :: tl -> split_result tl ((fst p)::l1) ((snd p)::l2)
		| _ -> List.rev l1,List.rev l2
	in 

	split_result lst [] []

let combine l1 l2 =

	let rec combine_result l1 l2 acc =
		(*let _ = F.printf " Recursion : %d " i in  debuging *)
		match (l1,l2) with
		| [],_ -> List.rev acc
		| p1::al,p2::bl -> combine_result al bl ((p1,p2)::acc)
		| _,_ -> List.rev acc
	in

	combine_result l1 l2 []

let print_ls name lst = 

	let _ = F.printf "%s : [ " name in
	let _ =	List.iter (fun x-> F.printf " %c" x ) lst in
	F.printf " ]\n"

let print_tp name lst =

	let _ = F.printf "%s : [ " name in
	let rec print lst =
		match lst with
		| [] -> F.printf " ]\n" 
		| p :: tl -> F.printf " ( %d , %d ) " (fst p) (snd p) ; print tl
	in

	print lst
(*test case *)
let _ =
	(*let l1 = [(1,3);(3,5);(1,2);(4,5)] in
	let _ = print_tp " Split this ! " l1 in*)
	let l2 = [('a','5');('b','c')] in
	let (a,b) = split l2 in
	let _ = print_ls "fst " a in
	let _ = print_ls "snd " b in
	(*let _ = F.printf " clear " in 
	let (al,bl) = split l1 in (* ([1;3;1;4],[3;5;2;5]) *)
	let _ =	print_ls "    first_split" al in (* [1;3;1;4] *)
	let _ = print_ls "    second_split" bl in (* [3;5;2;5] *)
	let _ = F.printf "\n" in*)
	let a = [2;4;8;3;1;0] in
	let b = [100;150;123;1000;25;31] in
	(*let _ = print_ls " A " a in
	let _ = print_ls " B " b in
	let _ = F.printf "\n" in*)
	let c = combine a b in (* [(2,100);(4,150);(8,123);(3,1000);(1,25);(0,31)] *)
	print_tp "    Combine A and B ! " c 
	
