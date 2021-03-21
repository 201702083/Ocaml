let rec separate xs =
    match xs with
  | x::y::tail ->
        let a,b = separate tail in
        x@a, y@b
  | x::[] -> x,[]
  | [] -> [],[]
let _ = 	
	let a = [("a","b");("b","z");("c","10");("4","z")] in
	let b = separate a in
	Format.printf "end"
