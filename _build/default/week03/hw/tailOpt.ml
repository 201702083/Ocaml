module F = Format
let split lst  =

	let rec split_result lst l1 l2 = 
		match lst with
		| p :: tl -> split_result tl ((fst p)::l1) ((snd p)::l2)
		| _ -> List.rev l1,List.rev l2
	in 

	split_result lst [] []

let combine l1 l2 =

	let rec combine_result l1 l2 acc =
		(*let _ = F.printf " Recursion : %d " i in  debuging *)
		match (l1,l2) with
		| [],_ -> List.rev acc
		| p1::al,p2::bl -> combine_result al bl ((p1,p2)::acc)
		| _ -> failwith " diff len "
	in

	combine_result l1 l2 []

let print_ls name lst =

        let _ = F.printf "%s : [ " name in
        let _ = List.iter (fun x-> F.printf " %s" x ) lst in
        F.printf " ]\n"

(*  for type matching *)
type mydef = I of int | S of string | F of float | C of char

let print_adjust d = 
	match d with
	| I i -> F.printf " %d" i 
        | S s -> F.printf " %s" s 
        | C c -> F.printf " %c" c 
        | F f -> F.printf " %f" f 
let print_genericList lst =
	let _ = F.printf " [" in
	let rec print lst =
		match lst with
		| [] -> F.printf " ]"
		| p :: tl -> print_adjust p; 
			     match tl with
			     | [] -> print tl
			     | _ -> F.printf "," ; print tl
	in
	print lst 

let rec print_genericPair lst =
	match lst with
	| [] -> F.printf " ]\n"
	| h :: tl -> F.printf " (" ; print_adjust (fst h); F.printf "," ; print_adjust (snd h) ; 
		     match tl with
	 	     | [] -> F.printf " ) ";print_genericPair tl
		     | _ -> F.printf " ), ";print_genericPair tl 
		
let pr1 (a,b) =
  let _ = F.printf " Pair of list : ( " in
  let _ = print_genericList a in
  let _ = F.printf "," in
  let _ = print_genericList b in
  F.printf " ) \n"

let pr2 lst =
	let _ = F.printf " List of pair : [ " in
	print_genericPair lst

let print_tp name lst =

	let _ = F.printf "%s : [ " name in
	let rec print lst =
		match lst with
		| [] -> F.printf " ]\n" 
		| p :: tl -> F.printf " ( %d , %d ) " (fst p) (snd p) ; print tl
	in

	print lst
(*test case *)
let _ =
	let l2 = [("hi","bye");("im tom","im going library")] in
        let (a,b) = split l2 in
        let _ = print_ls "fst " a in
        let _ = print_ls "snd " b in

	let a = [2;4;8;3;1;0] in
        let b = [100;150;123;1000;25;31] in
        let c = combine a b in (* [(2,100);(4,150);(8,123);(3,1000);(1,25);(0,31)] *)
	let _ = print_tp "      Combine A and B ! " c in 	

	(* Using mydef *)
	let a = [(I 1,S "hi"); (I 2,S "CSE"); (I 3, S "Ocaml")] in
	let (x,y) = split a in
	let b = [I 1;I 2;I 3;I 4;I 5] in
	let c = [C 'a';C 'c';C 'd';C 'e';C 'f'] in
	let e = combine b c in
	let _ = pr2 e in
	pr1 (x,y)
