module F = Format

(* practice & homework *)
let rec interp_e (s : Store.t) (e : Ast.expr) : Store.value = 
  match e with
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
    | Id (str) ->  begin
                        match (Store.find str s ) with
                        | FreezedV(exp, t) -> interp_e t exp
                        | x -> x
                    end
                    (* Store.find str s *)
    | LetIn (str,e1,e2) ->   interp_e (Store.insert str ( ( interp_e s e1 ) ) s ) e2 
    | RLetIn (str,e1,e2) -> begin
                                match (interp_e s e1) with 
                                | ClosureV(x1,e,s1) ->
                                    let rec s11 = (str, (Store.ClosureV(x1,e,s11))) :: s1 in
                                    interp_e s11 e2
                                | _ -> failwith (Format.asprintf "Not a function : %a" Ast.pp_e e1)
                            end
    | App (e1,e2) -> 
                begin
                  match ( interp_e s e1 ) with
                  | ClosureV (str , exp, t ) -> 
                                            begin
                                            interp_e (Store.insert str (Store.FreezedV(e2,s)) t ) exp 
                                            end
                  | _ -> failwith (Format.asprintf "Not a function: %a" Ast.pp_e e1)
                end
    | Fun (str,e1) -> (Store.ClosureV (str,e1,s)) 
    | Lt (e1, e2) -> 
                  begin
                    match ( ( interp_e s e1 ) , ( interp_e s e2 ) ) with
                    | NumV(a) , NumV(b) -> begin
                              if (a<b) then (Store.ClosureV("x",Fun("y",(Id "x")),s)) else (Store.ClosureV("x",Fun("y",(Id "y")),s))
                                          end
                    | _ -> failwith (Format.asprintf "Not a Comparable: %a %a" Ast.pp_e e1 Ast.pp_e e2)
                  end

(* practice & homework *)
let interp (p : Ast.fvae) : Store.value = 
    match p with 
  |Prog (e) -> interp_e [] e
  (* write your code *)