type t = (string * string)list
let empty = [] 
let rec add key value map =
	match map with
	| a :: tl -> 
		if ( (fst a) = key ) then ( (key,value) :: tl  ) 
		else a :: (add key value tl)
	|[] -> (key,value) :: map


let rec find key map =
	match map with
	| a :: tl -> if ( (fst a) = key ) then (snd a) else find key tl
	| [] -> failwith " No such key exists "
	
	
let rec erase key map =
	match map with
	| a :: tl -> if ( (fst a) = key ) then tl else a::(erase key tl)
	| [] -> failwith " No such key exists "
