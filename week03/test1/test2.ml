let rec  split_t lst =
	let split lst = 
		match lst with 
		| l1,_ -> Format.printf "%d " l1 in 
	match lst with
	| x::y::t -> let a,b = split_t t in
		     (split x)@a,(split y)@b
	| x::[] -> [x],[]
	| []::_ -> [],[]
let split lst =
	match lst with
	| l1 , _  -> Format.printf "%d " l1 
type mydef = Int of int | String of string | Char of char | Float of float

let print_type name lst =
	let _ = F.printf "%s : [" name in
	let rec print lst =
		match lst with 
		| [] -> F.printf " ]\n" 
		| p :: tl -> match p with 
			     | Int (int) -> F.printf " %d" int ; print tl
			     | String (string) -> F.printf " %s" string ; print tl
			     | Char (char) -> F.printf " %c" char; print tl
			     | Float (float) -> F.printf " %f" float; print tl
	
	in
	print lst

let check_type b = 
        match b with
  	| int -> F.printf "aaaaa %d " b
  	| string -> F.printf "bbbbb string "
  	| char -> F.printf "cccc char "
 
let _ =
	(*let a = [(1,2),(3,4)] in*)
	let b = [(3,5);(5,6)] in
	split_t b 
