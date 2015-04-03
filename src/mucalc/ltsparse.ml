exception SyntaxErrorTransitionSystem


let (&) f g x = f (g x)


let regex = Str.regexp


let toPairs xs =
    List.map (fun l -> match l with
                       | [x; y] -> (int_of_string x, y)
                       | _      -> raise SyntaxErrorTransitionSystem)
             xs


let parse s =
    let ss = (List.map (List.map String.trim & Str.split_delim (regex ",")) & Str.split (regex "\n")) s in
    let v = Array.make (List.length ss) [] in 
    let lts = List.mapi (fun i x ->
                            match x with
                            | [_; succ; ps] -> v.(i) <- (List.map String.trim & Str.split (regex ";")) ps;
                                               (toPairs & List.map (Str.split (regex " ") & String.trim) & Str.split (regex ";")) succ
                            | _             -> raise SyntaxErrorTransitionSystem)
                        ss in
    (Array.of_list lts, v)
