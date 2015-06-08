type label = string
type lts = (int * label) list array
type valuation = string list array

exception SyntaxErrorTransitionSystem


(* (&) : ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b *)
let (&) f g x = f (g x)


(* regex : string -> regexp *)
let regex = Str.regexp


(* to_pairs : string list list -> (int * string) list *)
let to_pairs xs =
    List.map (fun l -> match l with
                       | [x; y] -> (int_of_string x, y)
                       | _      -> raise SyntaxErrorTransitionSystem)
             xs


(* parse : string -> lts * valuation *)
let parse s =
    let ss = (List.map (List.map String.trim & Str.split_delim (regex ",")) & Str.split (regex "\n")) s in
    let v = Array.make (List.length ss) [] in 
    let lts = List.mapi (fun i x ->
                            match x with
                            | [_; succ; ps] -> v.(i) <- (List.map String.trim & Str.split (regex ";")) ps;
                                               (to_pairs & List.map (Str.split (regex " ") & String.trim) & Str.split (regex ";")) succ
                            | _             -> raise SyntaxErrorTransitionSystem)
                        ss in
    (Array.of_list lts, v)
