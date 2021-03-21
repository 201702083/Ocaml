module F = Format

let rec split lst  =
	let rec split_result lst l1 l2 = 
		match lst with
		| [] -> l1,l2
		| (lst1 , lst2) :: t -> split_result t l1@lst1 l2@lst2
		| _ -> F.printf " exception ! "
	in 
	let a = [] in
	let b = [] in
	split lst a b

let print_ls name lst = 
	let _ = F.printf "%s : [ " name in
	let _ = List.iter (fun x -> F.printf "%d " x ) lst in
	F.printf "]\n"

let _ =

	let (l1,l2) = split [(1,5);(2,6);(3,7);(4,8)] in
	print_ls "test1" l1
