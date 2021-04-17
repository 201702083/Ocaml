module F = Format

type t = (string * int) list
exception Empty of string 
let empty = [] 

let insert x n s = 
	let rec ins x n sh sl =
		match sl with
		| a :: tl ->
			if ( (fst a) = x ) then ( sh @ ( (x , n ) :: tl ) )
			else ( ins x n (List.rev (a::sh)) tl)
		|[] -> (x,n) :: (sh @ sl);
	in
	ins x n [] s

(*let rec find key map =
        match map with
        | a :: tl -> if ( (fst a) = key ) then (snd a) else find key tl
        | [] -> failwith " No such key exists "*)
let rec find x s = 
	match s with
	| a :: tl -> if ( (fst a) = x ) then (snd a) else find x tl
	| [] ->failwith ("Free identifier "^x)
