module F = Format

let rec interp_e (fenv : FEnv.t) (s : Store.t) (e : Ast.expr) : int = 
	(* write your code *)
	match e with
	|Num n -> n
	|Add (a,b) -> interp_e fenv s a + interp_e fenv s b
	|Sub (a,b) -> interp_e fenv s a - interp_e fenv s b
	|Id n -> Store.find n s
	|LetIn (n,e1,e2) -> interp_e fenv (Store.insert n (interp_e fenv s e1) s) e2
	|FCall (n,e) -> let (par,exp) = FEnv.find n fenv in 
			interp_e fenv s 

let interp_d (fd : Ast.fundef) : FEnv.t = 
	(* write your code *)
	match fd with
	|FDef (n,s,e) -> FEnv.insert n s e [] 

(* practice *)
let interp (p : Ast.f1vae) : int = 
	(* write your code *)
	match p with
	|Prog (fd,ex) ->  interp_e (interp_d fd) [] ex
