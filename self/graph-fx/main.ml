let char_to_int a = (int_of_char a) - 48

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

(*str -> (char list * string list ) *)
let separate str = 
	let rec sep lst f v sum =
		match lst with
		| a:: tl ->
			begin
				match a with
				|'0'..'9' -> sep tl f v (10*sum+(char_to_int a))
				|'x'|'X' -> if (sum != 0) then (sep tl f (v@[string_of_int sum]@[(String.make 1 a)]) 0) else (sep tl f (v@[(String.make 1 a)]) 0 )
				|'^'|'*'|'/'|'+'|'-' -> if (sum != 0) then
							(sep tl (f@[a]) (v@[string_of_int sum])  0)
							else
							sep tl (f@[a]) v 0
				| _ -> failwith ("Invalid input : "^(String.make 1 a))
			end
		| [] -> f,(v@[string_of_int sum])
	in
        let lst = clean (explode str) in
	sep lst [] [] 0

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
let pri lst = 
	let rec find lst max count index =
		match lst with
		| a :: tl ->
				begin
					if ( a > max )	then ( find tl a (count+1) count)
					else ( find tl max (count+1) index)
				end
		| [] -> index
	
	in
	find lst 0 0 0 

let priorcalc slst vlst =
	let cvlst = conv slst in

(*x 의 n 제곱 구하는 함수 *)
let square x n =
        let rec squ x n res =
                match n with
		| 0 -> 1
                | 1 -> res*x
                | _ -> squ x (n-1) (res*x)
        in
        squ x n 1	

(*리스트를 주어진 인덱스를 기준, 0~index / index+1~end 로 분리*)
let splitlst lst idx = 
	let rec spl lst idx count left right =
		match lst with
		| t :: tl -> if ( count < idx ) then ( spl tl idx (count+1) (left@[t]) right )
				else ( left, ([t]@tl) )
		| [] -> left,right
	in	
	spl lst idx 0 [] [] 
(*계산된 심볼 빼고, 계산값을 정수리스트에 넣는 함수*)


(* 순서가 높은 것을 계산하고 계산기호를 뺀 sym과 계산된 cons를 반환 /  반환값은 list 쌍 *)
let update f i result = 
	(*인덱스 기준으로 나누고, 두번째리스트의 헤더 심볼과 정수 두개를 가져온다. 가져올 때 원소를 지워줘야함.*)
	let sl,sr = splitlst (fst f) i in
	let cl,cr = splitlst (snd f) i in
	let s = match sr with
		| _ :: tl -> sl @ tl
		|_ -> failwith "Invalid"
	in
	let c = match cr with
		| _ :: _ :: tl -> cl @ ( result::tl)
                |_ -> failwith "Invalid"

	in
	s,c 
let str2int str = 
	let lst = clean (explode str) in
	let rec change lst sum =
		match lst with
		|a :: tl -> change tl (10*sum + (char_to_int a) )
		| [] -> sum
	in
	change lst 0

let xin strlst x=
	let rec change strlst intlst = 
		match strlst with
		| a::tl -> 
			begin
				match a with
				|"x"|"X" -> change tl (x::intlst)
				| _ -> change tl (str2int a :: intlst)
			end
		| [] -> List.rev (intlst)
	in
	change strlst []

(* 식 f (separate 로 나온 튜플) 에 x 를 넣어 계산을 하는 함수*)
let calc x f =

	(* let cons = xin (snd f) x in *)
	(*cons list를 int 리스트로 바꿔주며 이 떄 "X" or "x" 는 입력된 x 로 바꿔준다.*)
	let rec cal (s, c) =
		 (* 심볼기호 char 리스트를 우선순위 int 리스트로 *)
		let idx = pri (conv s) in (* 우선순위 int리스트에서 가장 큰 우선순위의 인덱스 int *)
		match s with
		| [] -> List.hd c
		| _ -> 
				begin
					match (List.nth s idx) with
                		|'-' -> cal (update (s,c) idx ((List.nth c idx) - (List.nth c (idx+1))))
						|'^' -> cal (update (s,c) idx (square (List.nth c idx) (List.nth c (idx+1))))
                		|'*' -> cal (update (s,c) idx ((List.nth c idx) * (List.nth c (idx+1))))
                		|'/' -> cal (update (s,c) idx ((List.nth c idx) / (List.nth c (idx+1))))
                		|'+' -> cal (update (s,c) idx ((List.nth c idx) + (List.nth c (idx+1))))
                		| _ -> failwith "fail input"
		    	end
	in
	let cons = xin (snd f) x in
	cal (fst f,cons)
		
	





	let result = 	
		match (List.nth symbol idx) with
		|'^' -> square (List.nth cons idx) (List.nth cons idx+1)
		|'*' -> (List.nth cons idx) * (List.nth cons idx+1)
		|'/' -> (List.nth cons idx) / (List.nth cons idx+1)
		|'+' -> (List.nth cons idx) + (List.nth cons idx+1)
		|'-' -> (List.nth cons idx) - (List.nth cons idx+1)
		| _ -> failwith "fail input"
	in
	let f = update f idx result in
	
	calc x f
	
	
(*    ----! 여기만 작성하면 끝 !-------- 좌표값을 구해서 그래프로 시각화 하는 함수 --------- *)
(* min ~ max 구간을 계산해 결과값을 도출하는 함수*)
let graph f min max =
	let rec point f min max lst =
		if ( min <= max) then ( point f (min+1) max ((calc f min)::lst) )
		else (List.rev lst)
	in
	point f min max []
