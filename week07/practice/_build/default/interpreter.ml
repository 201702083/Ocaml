module F = Format
exception Empty of string
(* practice *)
let rec interp (s : Store.t) (e : Ast.vae) : int =
	match e with 
	|Num (n) -> n (*-> int 뱉음 *)
	|Add (a,b) -> (interp s a) + (interp s b)
	|Sub (a,b) -> (interp s a) - (interp s b)
	|Id x -> Store.find x s
	|LetIn (str,a,b) -> interp (Store.insert str (interp s a) s) b
