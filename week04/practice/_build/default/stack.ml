type t = int list 

let push e s = e :: s 



let pop s = 
	match s with
	|_ :: t -> t 
	|[] -> failwith " stack is empty "
let _ =
	let a = [] in
	let a_1 = push 1 a in
	(*let a_1_2 = push 2 a_1 in*)
	let b = pop a_1 in
	pop b


