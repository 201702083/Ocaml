module F = Format

type t = 
  | Int of int
  | Var of string

type state = 
  | S0  (* 첫 문자 입력 -> 정수인지 변수인지 판별 *)
  | S1  (* '-'인 경우 *)
  | S2  (* 정수인 경우 *)
  | S3  (* 문자인 경우 *)

let char_to_int c = (int_of_char c) - 48 
let char_to_str c = Char.escaped c

(* lex : char list -> t *)
let lex chars = 
  (* lex_impl : state -> char list -> int -> string -> t *)
	
	let rec lex_impl state chars v s =
		match state with
		|S0->
			begin
				match chars with
				|h::t ->
					begin
						match h with
						|'-' -> begin
								match (lex_impl S1 t v s) with
								| Int n -> Int (-n)
								| _ -> failwith "Not a valid integer or a valid variable"
							end
						|'0' .. '9' -> lex_impl S2 t (char_to_int h) s
						|'a' .. 'z' -> lex_impl S3 t v (s^(char_to_str h))
						| _ -> failwith "Not a valid integer or a valid variable"
					end
				|[] -> failwith "Not a valid integer or a valid variable"
			end
		|S1->   (* - 가 들어옴 *)
			begin
				match chars with
				|h::t->
					begin
						match h with
						|'0' .. '9' -> lex_impl S2 t (v*10 + (char_to_int h)) s 
						|_ -> failwith "Not a valid integer or a valid variable"
					end
				|[] -> failwith "Not a valid integer or a valid variable"
			end
		|S2->  (* 정수가 들어옴*)
			begin
				match chars with
				|h::t ->
					begin
						match h with
						|'0' .. '9' -> lex_impl S2 t (v*10 + (char_to_int h)) s
						| _ -> failwith "Not a valid integer or a valid variable"
					end
				|[] -> Int v
			end
		|S3->  (* 문자가 들어옴*)
			begin
				match chars with
				|h::t->
					begin
						match h with
						|'0' .. '9' -> lex_impl S3 t v ( s^( char_to_str h))
						|'a' .. 'z' ->lex_impl S3 t v ( s^( char_to_str h))
						|'A' .. 'Z' ->lex_impl S3 t v ( s^( char_to_str h))
						|'\'' ->lex_impl S3 t v ( s^( char_to_str h))
						|'_' ->lex_impl S3 t v ( s^( char_to_str h))
						|_ -> failwith "Not a valid integer or a valid variable"
					end
				|[] -> Var s
			end
	in
	lex_impl S0 chars 0 "" 
  (* write your code *)

let pp fmt v = 
  match v with
  | Int i -> F.fprintf fmt "Int %d" i
  | Var x -> F.fprintf fmt "Var %s" x 
