type ae = 
	| Num of int
	| Fun of string * ae * ae
	| Add of ae*ae
	| Sub of ae*ae
	| Mul of ae*ae
	| Neg of ae

(*ex Fun ( Add(Mul(Num 3 , X),Num 8)  f(x) = 3 * x+8  *)

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
(*str -> (char list * char list ) *)
let separate str = 
	let rec sep lst f v  =
		match lst with
		| a:: tl ->
			begin
				match a with
				|'0'..'9'|'x'|'X' -> sep tl f ([a]@v)
				|'^'|'*'|'/'|'+'|'-' -> sep tl ([a]@f) v
				| _ -> failwith ("Invalid input : "^(String.make 1 a))
			end
		| [] ->  List.rev f, List.rev v
		
		
	in
        let lst = clean (explode str) in
	sep lst [] [] 

(* 심볼리스트를 우선순위 리스트로 바꿈*)
let conv lst =
	let rec convt lst newlst =

		match lst with
		| a:: ls ->
			begin
				match a with
				| '^' -> convt ls (100 ::newlst)
				| '*'|'/' -> convt ls (50::newlst)
				| '+'|'-' -> convt ls (1::newlst)
				| _ -> failwith "Invalid element"
			end 
			
		|[] -> List.rev newlst
	in
	convt lst []
let priorcalc slst vlst =
	let cvlst = conv slst in
	
(*x, 심볼리스트, 상수리스트 를 가지고 계산을 하는 함수*)
let calc x symbol cons =
	let rec 

(* min ~ max 구간을 계산해 결과값을 도출하는 함수*)
let graph f min max =
 	 
	begin		
		match f with
		|
		|
		|
	end
