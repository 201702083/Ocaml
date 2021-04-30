module F = Format

type t = (string * int) list

let empty = []

let insert x n s = (* write your code *)
        let rec ins x n sh sl =
                match sl with
                | a :: tl ->
                        if ( (fst a) = x ) then ( sh @ ( (x , n ) :: tl ) )
                        else ( ins x n (List.rev (a::sh)) tl)
                |[] -> (x,n) :: (sh @ sl);
        in
        ins x n [] s


let rec find x s = 
  (* write your code *)
	match s with
	        | a :: tl -> if ( (fst a) = x ) then (snd a) else find x tl
	        | [] ->failwith ("Free identifier "^x)
let pp fmt s = 
  let rec pp_impl fmt s = 
    match s with
    | [] -> F.fprintf fmt "]"
    | (x, n) :: t -> F.fprintf fmt "(%s, %d) %a" x n pp_impl t
  in
  F.fprintf fmt "[ %a" pp_impl s
