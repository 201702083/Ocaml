type data = 
	|Int of int
	|Float of float

type ae = 
	| Num of data
	| Add of ae * ae
	| Sub of ae * ae
	| Mul of ae * ae
	| Neg of data

(* string -> char list *)
let explode s = List.init (String.length s) (String.get s)	
(* delete white space *)
let clean lst =
	let rec cl lst res =
		match lst with
		| a::tl -> 
			begin
				match a with
				|' ' -> cl tl res
				| _ -> cl tl (a::res)
			end	
		| [] -> List.rev res
	in
	cl lst []

(* char list  -> int  *)
let calcul lst =
	(*char list -> int -> int *)
	let rec cal lst l r  =
		match lst with
		| a :: tl ->
				begin
					match a with
					|'0'..'9' -> cal tl (10*l + a) 0
					|'+' ->  (cal tl 0 l+r)
					|'-' ->  (cal tl 0 l-r)
					| _ -> failwith "Not build"
				end
		| [] -> r
	in
	cal lst 0 0 


let _ = 
        let _ = print_endline "계산식을 입력하세요" in

        let str = read_line() in
        let alst = explode str in
        let lst = clean alst in
        (* 공백을 제외한 입력들의 char list 완성 *)
	 
	calcul lst
