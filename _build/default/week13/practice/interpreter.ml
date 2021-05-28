module F = Format

(* practice & homework *)
let rec interp_e (e : Ast.expr) (s : Store.t) : Store.value = 
  match e with
  | Num i -> NumV(i)
  | Var str -> Store.find str s
  | Bool b -> BoolV(b)
  | Add (e1,e2) -> 
                    begin 
                    match ((interp_e e1 s), (interp_e e2 s)) with
                    | NumV(a) , NumV(b) -> NumV( a+b )
                    | _ -> failwith (Format.asprintf "Invalid addition: %a + %a" Ast.pp_e e1 Ast.pp_e e2)
                    end
  | Sub (e1,e2) -> 
                     begin
                    match ((interp_e e1 s), (interp_e e2 s)) with
                    | NumV(a) , NumV(b) -> NumV( a-b )
                    | _ -> failwith (Format.asprintf "Invalid subtraction: %a + %a" Ast.pp_e e1 Ast.pp_e e2)
                    end
  | Lt (e1,e2) -> 
                    begin
                    match ((interp_e e1 s), (interp_e e2 s)) with
                    | NumV(a) , NumV(b) -> if (a<b) then (BoolV(true)) else (BoolV(false))
                    | _ -> failwith (Format.asprintf "Invalid less-than: %a < %a" Ast.pp_e e1 Ast.pp_e e2)
                     end
  | Gt (e1,e2) -> 
                    begin
                    match ((interp_e e1 s), (interp_e e2 s)) with
                    | NumV(a) , NumV(b) -> if (a>b) then (BoolV(true)) else (BoolV(false))
                    | _ -> failwith (Format.asprintf "Invalid greater-than: %a > %a" Ast.pp_e e1 Ast.pp_e e2)
                     end
  | Eq (e1,e2) -> 
                    begin
                    match ((interp_e e1 s), (interp_e e2 s)) with
                    | NumV(a) , NumV(b) -> if (a==b) then (BoolV(true)) else (BoolV(false))
                    | _ -> failwith (Format.asprintf "Invalid equal-to: %a == %a" Ast.pp_e e1 Ast.pp_e e2)
                     end
  | And (e1,e2) -> 
                    begin
                    match ((interp_e e1 s), (interp_e e2 s)) with
                    | BoolV(a) , BoolV(b) -> 
                                            begin
                                            match a,b with
                                            | true,true -> BoolV(true)
                                            | _ -> BoolV(false)
                                            end
                    | _ -> failwith (Format.asprintf "Invalid logical-and: %a && %a" Ast.pp_e e1 Ast.pp_e e2)
                     end
  | Or (e1,e2) -> 
                    begin
                    match ((interp_e e1 s), (interp_e e2 s)) with
                    | BoolV(a) , BoolV(b) -> 
                                            begin
                                            match a,b with
                                            | false,false -> BoolV(false)
                                            | _ -> BoolV(true)
                                            end
                    | _ -> failwith (Format.asprintf "Invalid logical-or: %a || %a" Ast.pp_e e1 Ast.pp_e e2)
                     end



(* practice & homework *)
let rec interp_s (stmt : Ast.stmt) (s : Store.t) : Store.t = 
    let rec execlst stmtlst s1 = 
        match stmtlst with
        | a :: tl -> execlst tl (interp_s a s1)
        | [] -> s1
    in

    match stmt with
    | AssignStmt ( str , e ) -> Store.insert str (interp_e e s) s
    | IfStmt (e, true_stmts, false_stmts_opt) ->
            begin
            match false_stmts_opt with
            | None -> 
                        begin
                        match (interp_e e s) with
                        | BoolV(true) ->  
                                        begin
                                            match true_stmts with
                                            | [] -> s
                                            | _ -> execlst true_stmts s
                                        end 

                        | BoolV(false) -> s
                        | _ -> failwith (Format.asprintf "Not a boolean : %a" Ast.pp_e e)
                        end

            | Some false_stmts -> 
                        begin
                        match (interp_e e s) with
                        | BoolV(true) -> 
                                        begin
                                            match true_stmts with
                                            | [] -> s
                                            | _ -> execlst true_stmts s
                                        end  
                        | BoolV(false) -> 
                                        begin
                                            match false_stmts with
                                            | [] -> s
                                            | _ -> execlst false_stmts s
                                        end 
                        | _ -> failwith (Format.asprintf "Not a boolean : %a" Ast.pp_e e)
                        
                        end
            end

(* practice & homework *)
let interp (p : Ast.program) : Store.t = 
    let rec interp_ss stmtlst t =
        match stmtlst with
        | a :: tl -> interp_ss tl ( interp_s a t )
        | [] -> t
    in

    match p with 
    | Program s -> interp_ss s []
