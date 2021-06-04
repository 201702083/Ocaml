module F = Format

(* practice & homework *)
let rec interp_e (e : Ast.expr) ((env, mem) : Env.t * Mem.t) : Mem.value = 
  let s = (env,mem) in
   match e with
  | Num (i) -> NumV(i)
  | Var (str) -> Mem.find (Env.find str env) mem
  | Bool (b) -> BoolV(b)
  | Ref (str) -> AddressV(Env.find str env)
  | Deref (str) -> begin  
                    match (Mem.find (Env.find str env) mem) with
                    | AddressV (add) -> Mem.find add mem
                    | _ -> failwith (Format.asprintf "Not a memory address : %a" Ast.pp_e e)

                    end
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
let rec interp_s (stmt : Ast.stmt) ((env, mem) : Env.t * Mem.t) : Env.t * Mem.t = 
    let rec execlst stmtlst (e1,m1) = 
        match stmtlst with
        | a :: tl -> execlst tl (interp_s a (e1,m1))
        | [] -> e1,m1
    in
    let s = (env,mem) in
    match stmt with
    
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
    | StoreStmt (e1,e2)-> begin  
                         match (interp_e e1 s) with
                         | AddressV ( add ) -> (env, Mem.insert add (interp_e e2 s ) mem)
                         |  _ -> failwith (Format.asprintf "Not a memory address : %a" Ast.pp_e e1)
                        end
    | VarDeclStmt (str) -> begin
                        match (Env.mem str env) with
                        |  (true) -> failwith (Format.asprintf "%s is already declared." str)
                        |  (false) -> ((Env.insert (str) (Env.new_address()) (env) ), mem)

                            end
    | WhileStmt (e,stmtlst) -> begin
                                let rec whileloop e s stmtlist =
                                    match (interp_e e s) with
                                    | BoolV(true) -> whileloop e (execlst stmtlist s) stmtlist
                                    | BoolV(false) -> s
                                    | _ -> failwith (Format.asprintf "Not a boolean : %a" Ast.pp_e e)
                                in
                                whileloop e s stmtlst
                               end
                        

(* practice & homework *)
let interp (p : Ast.program) : Env.t * Mem.t = 
    let rec interp_ss stmtlst (envt,memt) =
        match stmtlst with
        | a :: tl -> interp_ss tl ( interp_s a (envt,memt) )
        | [] -> envt,memt
    in

    match p with 
    | Program s -> interp_ss s (Env.empty,Mem.empty)
 