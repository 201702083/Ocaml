type unop = Neg
type binop = Add | Sub | Mul | Div

type exp = 
	 Constant of int
	|Unary of unop * exp
	|Binary of exp * binop * exp

let rec eval e =
	match e with (*e는 exp 타입 *)
	| Constant (i) -> (i)
	| Unary (Neg,exp) -> -( eval exp )
	| Binary (i1,Add,i2) ->( eval i1) + eval i2
	| Binary (i1,Sub,i2) ->( eval i1) - eval i2
	| Binary (i1,Mul,i2) ->( eval i1) * eval i2
	| Binary (i1,Div,i2) ->( eval i1) / eval i2

