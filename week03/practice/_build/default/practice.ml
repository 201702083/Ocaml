module F = Format
type unary_number = Z | S of unary_number

let rec add n m = 
	match n with
	| Z -> m (* z 에 m 대입 *)
	| S(unary_number) -> S(add (unary_number) (m)) 

let mul n m  =
	let rec mul_acc n m acc =
		match m with 
		| Z -> acc
		| S(unary) -> mul_acc n unary ( add (acc) (n) )
	in 
	mul_acc n m Z

let rec print_un fmt f =
	match f with
	| Z -> F.fprintf fmt "Z"
	| S(unary) -> F.fprintf fmt "( S %a )" print_un unary

let _ =
	let a = add (S(S(S Z))) (S(S Z)) in
	let b = mul (S(S(S Z))) (S(S(S Z))) in
let _= F.printf " a = %a\n" print_un a in 
F.printf " b = %a\n" print_un b


