let empty = [] 

let print_map map =
	let _ = Format.printf " [ " in
	let rec print map =
	match map with
	| [] -> Format.printf " ]\n"
	| a :: tl -> match tl with
		     | [] ->Format.printf "( %s %s )" (fst a) (snd a); print tl
		     | _ :: _ -> Format.printf "( %s %s ); " (fst a) (snd a); print tl
	in
	print map


let _=
let empty_map = ListMap.empty in (* empty_map = [] *)
let map1 = ListMap.add "name" "kihoon" empty_map in (* map1 = [(name, kihoon)] *)
let map1' = ListMap.add "age" "23" map1 in (* map1' = [(age, 23); (name, kihoon)] *)
let map1'' = ListMap.add "city" "Daejeon" map1' in (* map1'' = [(city, Daejeon); (age, 23);
(name, kihoon)] *)
let name = ListMap.find "name" map1'' in (* name = kihoon *)
let _ = Format.printf "name : %s\n" name in
let city = ListMap.find "city" map1'' in (* city = Daejeon *)
let _ = Format.printf "city : %s\n" city in
let age = ListMap.find "age" map1'' in (* age = 23 *)
let _ = Format.printf "age : %s\n" age in
let map2 = ListMap.erase "age" map1'' in (* map2 = [(city, Daejeon); (name, kihoon)] *)
let _ = print_map map2 in
let map2' = ListMap.erase "name" map1'' in (* map2 = [(city, Daejeon); (age, 23)] *)
let _ = print_map map2' in
let map2'' = ListMap.erase "city" map1'' in (* map2 = [(age, 23); (name, kihoon)] *)
let _ = print_map map2'' in
Format.printf " end\n "
