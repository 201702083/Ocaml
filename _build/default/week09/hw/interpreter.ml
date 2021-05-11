module F = Format

let rec interp_e (fenv : FEnv.t) (s : Store.t) (e : Ast.expr) : int = 
  (* write your code *)
    match e with
    |Num n -> n
    |Add (a,b) -> interp_e fenv s a + interp_e fenv s b
    |Sub (a,b) -> interp_e fenv s a - interp_e fenv s b
    |Id n -> Store.find n s
    |LetIn (n,e1,e2) -> interp_e fenv (Store.insert n (interp_e fenv s e1) s) e2
    |FCall (n,elst) -> 
                      begin
                        let (plst, exp) = FEnv.find n fenv in
                        let rec letinn pl el s = 
                          match pl,el with  
                          | a :: ptl, b :: etl -> letinn ptl etl (Store.insert a (interp_e fenv s b) s)
                          |[],[] -> s
                          | _,_ -> failwith "Arguments mismatch"
                        in
                        interp_e fenv (letinn plst elst s) exp
                      end
                       
                      
let interp_d (fenv : FEnv.t) (fd : Ast.fundef) : FEnv.t = 
  (* write your code *)
    match fd with
    |FDef (n,s,e) -> FEnv.insert n s e fenv
(* practice *)
let interp (p : Ast.f1vae) : int = 
  (* write your code *)
    match p with 
    |Prog (a,b) -> let fdlst = a in let ex = b in
    
    let fdef fl  =
        let rec ins fl env = 
          match fl with
          |a::tl ->  ins tl (interp_d env a) (*  env ¹İÈ¯ *)
          |[] -> env
        in
        ins fl []
    in

    interp_e (fdef fdlst) [] ex
