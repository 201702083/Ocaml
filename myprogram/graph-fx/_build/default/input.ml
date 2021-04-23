let char_to_int a = (int_of_char a) - 48

(* string -> char list *)
let explode s = List.init (String.length s) (String.get s)

(* delete white space *)
let clean lst =
        let rec cl lst res =
                match lst with
                | a::tl ->
                        begin
                                match a with
                                |' ' -> cl tl res
                                | _ -> cl tl (a::res)
                        end
                | [] -> List.rev res
        in
        cl lst []