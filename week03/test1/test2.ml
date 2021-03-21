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

let _ =
	(*let a = [(1,2),(3,4)] in*)
	let b = [(3,5);(5,6)] in
	split_t b 
