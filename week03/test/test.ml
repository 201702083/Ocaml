let rec separate xs = 
	match xs with
	| x::y::tail ->
		let a,b = separate tail in
		x::a,y::b
	| x::[] -> [x],[]
	| [] -> [],[];;


let _ =
	let rec print_tl (l1,l2) =
		match l1 with	
		| [] -> print_tl (l2,[])
	in	
	let print_ls lst = 
		let _ = Format.printf " [ " in 
		let _ = List.iter (fun x -> Format.printf "%d " x ) lst in
		Format.printf "]\n" 
	in

	let a = [1;2;3;4;5;6;7;8;9] in
	let b = separate [(1,2);(2,3);(3,4);(5,6)] in
	let _ = print_tl b in
	print_ls a ;;
