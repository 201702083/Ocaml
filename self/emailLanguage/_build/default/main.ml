type state = S0 | S1 | S2 | S3 

let rec list_car ch = match ch with
    | "" -> []
    | ch -> (String.get ch 0 ) :: (list_car (String.sub ch 1 ( (String.length ch)-1) ) )  

let char_to_str c = Char.escaped c

let email s  =
	let lst = list_car s in
	let rec check state lst str =
		begin
			match state with
			|S0 -> 
				begin
					match lst with
					| a::tl -> 
						begin
							match a with
							|'0'..'9' -> check S0 tl (str^(char_to_str a))
							|'a'..'z' -> check S0 tl (str^(char_to_str a))
							|'A'..'Z' -> check S0 tl (str^(char_to_str a))
							|'.' -> check S0 tl (str^(char_to_str a))
							|'@' -> check S1 tl (str^(char_to_str a))
							| _ -> failwith (str^(char_to_str a)^" !! "^(char_to_str a)^" <- Invalid email form")
						end
					| [] -> failwith "Empty String"
				end
			|S1 ->
				begin
					match lst with
					| a::tl ->
						begin
							match a with
							|'a'..'z' -> check S1 tl (str^(char_to_str a))
							| '.' -> check S2 tl (str^(char_to_str a))
							| _ -> failwith (str^(char_to_str a)^" !! "^(char_to_str a)^" <- Invalid email form")
						end
					| [] -> failwith (str^" !!  <- Last of email is empty")
				end
			|S2 ->
				begin
					match lst with
					| a::tl -> 
						begin
							match a with
							|'a'..'z' -> check S3 tl (str^(char_to_str a))
							| _ -> failwith (str^(char_to_str a)^" !! "^(char_to_str a)^" <- Invalid email end form")
						end
					| [] -> failwith (str^" !! <- End of email is incomplete")
				end
			|S3 ->
				begin
					match lst with
					| a::tl ->
						begin
							match a with
							|'a'..'z' -> check S3 tl (str^(char_to_str a))
							|'.' -> check S2 tl (str^(char_to_str a))
							| _ -> failwith (str^(char_to_str a)^" !! "^(char_to_str a)^ " <- Invalid input end form")
						end
					| [] -> str
				end
		end
	in
	check S0 lst ""
let pp fmt v =  Format.fprintf fmt "%s" v

let _ =
	let a = email "chs29359685@gmail.com" in
	let _ = Format.printf "Email : %a\n" pp a in
	email "i7722@naver@"

