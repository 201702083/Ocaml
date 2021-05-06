module F = Format

type t = (string * (string list * Ast.expr)) list

let empty = []

let insert x plist body s = (* write your code *)
    let rec ins x p b sh sl =
      match sl with
      | a :: tl -> 
        if ( (fst a ) = x ) then (sh @ ( (x , (p,b) ) :: tl ) )
        else ( ins x p b ( List.rev (a::sh)) tl ) 
      |[] -> (x,(p,b)) :: (sh@sl);
    in
    ins x plist body [] s

let rec find x s = 
  (* write your code *)
    match s with
    | a :: tl -> if ( (fst a) = x ) then (snd a)
        else find x tl
    | [] -> failwith ("Free identifier F  "^x)

let pp fmt s = 
  let rec pp_impl fmt s = 
    match s with
    | [] -> F.fprintf fmt "]"
    | (x, (p, e)) :: t -> F.fprintf fmt "(%s, (%s, %a)) %a" x p Ast.pp_e e pp_impl t
  in
  F.fprintf fmt "[ %a" pp_impl s
