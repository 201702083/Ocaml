let _ = 
	let lst = [1;2;3;4] in
	let lste = List.map (fun x -> x+1 ) lst in
	let res = List.fold_left (fun i x -> i + x ) 0 lste in
Format.printf "Res : %d\n " res

