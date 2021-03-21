module F = Format

(* test cases *)
let _ =
  let open Arith in
  let a = eval (Binary (Constant 1, Add, Constant 3)) in (* a = 4 *)  (*  1 + 3  = 4 *)
  let _ = F.printf "a = %d\n" a in
  let b = eval (Binary (Constant 5, Add, (Unary (Neg, (Constant 3))))) in (* b = 2 *) (* 5 + ( -1 * 3 ) 5 -3 = 2   *)
  let _ = F.printf "b = %d\n" b in
  let c = eval (Unary (Neg, (Binary (Constant 2, Mul, (Unary (Neg, (Constant 7))))))) in (* c = 14 *)  (* ( -1 * ( 2 mul ( -1*7)))  2 x 7 = 14  *)
  F.printf "c = %d\n" c





