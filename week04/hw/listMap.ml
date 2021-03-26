type t = (string * string)list
let empty = [] 
let add key value map =
	let rec add_rec key value mh ml = 
	match ml with
	| a :: tl -> 
		if ( (fst a) = key ) then ( (key,value) :: tl  ) 
		else (add_rec key value (a::mh) tl)
	|[] -> (key,value) :: map
	in
	add_rec key value [] map


let rec find key map =
	match map with
	| a :: tl -> if ( (fst a) = key ) then (snd a) else find key tl
	| [] -> failwith " No such key exists "
	
	
let erase key map =
	let rec erase_rec key mh ml =
	match ml with
	| a :: tl -> if ( (fst a) = key ) then mh@tl else (erase_rec key (a::mh) tl)
	| [] -> failwith " No such key exists "
	in
	erase_rec key [] map
