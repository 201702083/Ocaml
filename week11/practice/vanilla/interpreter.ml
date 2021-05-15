module F = Format

(* practice & homework *)
let rec interp_e (s : Store.t) (e : Ast.expr) : Store.value = 
  (* write your code *)
  match e with
    | Bool (i) -> BoolV(i)
    | Num (i) -> NumV i
    | Add (e1,e2) -> begin 
                    match ( ( interp_e s e1 ) , ( interp_e s e2 ) ) with
                    | NumV (a), NumV(b) -> NumV(a+b)
                    | _ -> failwith (Format.asprintf "Invalid addition: %a + %a" Ast.pp_e e1 Ast.pp_e e2)
                    end
    | Sub (e1,e2) ->begin  
                    match ( ( interp_e s e1 ) , ( interp_e s e2 ) ) with
                    | NumV(a),NumV(b) -> NumV(a-b)
                    | _ -> failwith (Format.asprintf "Invalid subtraction: %a - %a" Ast.pp_e e1 Ast.pp_e e2)
                    end
    | Id (str) ->  Store.find str s
    | LetIn (str,e1,e2) ->   interp_e (Store.insert str ( ( interp_e s e1 ) ) s ) e2 
    | App (e1,e2) -> 
                begin
                  match ( interp_e s e1 ) with
                  | ClosureV (str , exp, t ) -> 
                                            begin
                                            interp_e (Store.insert str (interp_e s e2) t ) exp 
                                            end
                  | _ -> failwith (Format.asprintf "Not a function: %a" Ast.pp_e e1)
                end
    | Fun (str,e1) -> (Store.ClosureV (str,e1,s)) 
    | Cond (e, t, f) -> 
                  begin
                    match (interp_e s e) with
                    | BoolV(i) -> 
                              begin
                                match i with
                                | true -> interp_e s t
                                | false -> interp_e s f
                              end 
                    | _ -> failwith (Format.asprintf "Not a bool value: %a" Ast.pp_e e)
                  end
    | Lt (e1, e2) -> 
                  begin
                    match ( ( interp_e s e1 ) , ( interp_e s e2 ) ) with
                    |NumV(a),NumV(b) -> if ( a < b) then (Store.BoolV(true)) else (Store.BoolV(false))
                    | _ -> failwith (Format.asprintf "Not a Comparable: %a %a" Ast.pp_e e1 Ast.pp_e e2)
                  end
(* practice & homework *)
let interp (p : Ast.fvae) : Store.value = 
  match p with 
  |Prog (e) -> interp_e [] e
  (* write your code *)
