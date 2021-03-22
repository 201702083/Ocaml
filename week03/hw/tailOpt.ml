module F = Format
let split lst  =

	(*let split_fst i = 
		match i with
		| a,_ -> a
	in
	let split_snd i =
		match i with
		| _,a -> a
	in*)
	let rec split_result lst l1 l2 = 
		match lst with
		| p :: tl -> split_result tl ((fst p)::l1) ((snd p)::l2)
		| _ -> List.rev l1,List.rev l2
	in 
	split_result lst [] []

let combine l1 l2 =
	let rec combine_result l1 l2 acc i =
		let _ = F.printf " Recursion : %d " i in
		match (l1,l2) with
		| [],_ -> List.rev acc
		| p1::al,p2::bl -> combine_result al bl ((p1,p2)::acc) (i+1)
		| _,_ -> List.rev acc
	in
	combine_result l1 l2 [] 0

let print_ls name lst = 
	let _ = F.printf "%s : [ " name in
	let _ =	List.iter (fun x-> F.printf " %d " x ) lst in
	F.printf " ]\n"
let print_tp name lst =
	let _ = F.printf "%s : [ " name in
	let rec print lst =
		match lst with
		| [] -> F.printf " ]\n" 
		| p :: tl -> F.printf " ( %d , %d ) " (fst p) (snd p) ; print tl
	in
	print lst
let _ =
	let l1 = [(1,3);(3,5);(1,2);(4,5)] in
	let (al,bl) = split l1 in
	let l = [1;2;3;4;5;6;7;8] in
	let _ = print_ls "f print_ls test" l in
	let _ =	print_ls "first_split" al in
	let _ = print_ls "second_split" bl in

	let a = [1;2;3] in
	let b = [4;5;6] in
	let c = combine a b in
	print_tp "c " c 
	
