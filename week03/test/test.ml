let split list n =
	let rec aux i acc = function
	| [] -> List.rev acc, []
	| h :: l -> if i = 0 then List.rev acc , l
			else aux (i-1) (h::acc) l in
	aux n [] list
let _ =
	let print_tup lst = 
		let _ = Format.printf " [ " in
 		let _ = List.iter (fun x -> Format.printf "( %d ) " x ) lst in	
		Format.printf "]\n"
	in
	let print_ls lst = 
		let _ = Format.printf " [ " in 
		let _ = List.iter (fun x -> Format.printf "%d " x ) lst in
		Format.printf "]\n" 
	in

	let a = [1;2;3;4;5;6;7;8;9] in
	let b = [(1,2);(2,3);(3,4);(5,6)] in
	let _ = print_tup b in
	print_ls a ;;
