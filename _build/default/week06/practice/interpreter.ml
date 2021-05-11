module F = Format

(* practice *)
let rec interp (e : Ast.ae) : int = 
  (* write your code *)
	match e with
	|Num (n) -> n
	|Add (a,b) -> interp(a) + interp(b)
	|Sub (a,b) -> interp(a) - interp(b)
