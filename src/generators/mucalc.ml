type label = string

type lts = (int * label) list array

type muexpr =
    | TT
    | FF
    | Atom of string
    | Var of string
    | Neg of muexpr
    | Con of muexpr * muexpr
    | Dis of muexpr * muexpr
    | ForAll of label * muexpr
    | Exists of label * muexpr
    | LFP of string * muexpr
    | GFP of string * muexpr


module VarMap = Map.Make(String)
module PrioMap = Map.Make(String)


exception NegationFailure


(* composite : muexpr -> bool *)
let composite = function
    | Neg _
    | Con _
    | Dis _ 
    | ForAll _
    | Exists _
    | LFP _
    | GFP _     -> true
    | _         -> false


(* string_of_muexpr : muexpr -> string *)
let rec string_of_muexpr = function
    | TT             -> "TT"
    | FF             -> "FF"
    | Atom p         -> p
    | Var x          -> x
    | Neg e          -> if composite e
                        then "~("^(string_of_muexpr e)^")"
                        else "~"^(string_of_muexpr e)
    | Con (e1, e2)   -> (if composite e1
                         then "("^(string_of_muexpr e1)^") and "
                         else (string_of_muexpr e1)^" and ")^
                        (if composite e2
                         then "("^(string_of_muexpr e2)^")"
                         else (string_of_muexpr e1))
    | Dis (e1, e2)   -> (if composite e1
                         then "("^(string_of_muexpr e1)^") or "
                         else (string_of_muexpr e1)^" or ")^
                        (if composite e2
                         then "("^(string_of_muexpr e2)^")"
                         else (string_of_muexpr e1))
    | ForAll (l, e)  -> if composite e
                        then "["^l^"] ("^(string_of_muexpr e)^")"
                        else "["^l^"] "^(string_of_muexpr e)
    | Exists (l, e)  -> if composite e
                        then "<"^l^"> ("^(string_of_muexpr e)^")"
                        else "<"^l^"> "^(string_of_muexpr e)
    | LFP (x, e)     -> if composite e
                        then "mu "^x^".("^(string_of_muexpr e)^")"
                        else "mu "^x^"."^(string_of_muexpr e)
    | GFP (x, e)     -> if composite e
                        then "nu "^x^".("^(string_of_muexpr e)^")"
                        else "nu "^x^"."^(string_of_muexpr e)


(* string_of_muexpr : muexpr -> int -> string *)
let string_of_state e s =
    "("^(string_of_muexpr e)^", "^(string_of_int s)^")"


(* eg_to_pg : ((muexpr * int) * int list) array -> string list array -> paritygame *)
let eg_to_pg eg v prio =
    let p_max = 10002 in
    Array.mapi
        (fun i ((e, s), succ) ->
            match e with
            | TT           -> (p_max, 0, Array.of_list (i::succ), Some (string_of_state e s))
            | FF           -> (p_max + 1, 1, Array.of_list (i::succ), Some (string_of_state e s))
            | Atom p       -> if List.mem p v.(s)
                              then (p_max, 0, Array.of_list (i::succ), Some (string_of_state e s))
                              else (p_max + 1, 1, Array.of_list (i::succ), Some (string_of_state e s))
            | Neg (Atom p) -> if List.mem p v.(s)
                              then (p_max + 1, 1, Array.of_list (i::succ), Some (string_of_state e s))
                              else (p_max, 0, Array.of_list (i::succ), Some (string_of_state e s))
            | Var x        -> (PrioMap.find x prio, 0, Array.of_list succ, Some (string_of_state e s))
            | Neg _        -> raise NegationFailure
            | Con _        -> (0, 1, Array.of_list succ, Some (string_of_state e s))
            | Dis _        -> (0, 0, Array.of_list succ, Some (string_of_state e s))
            | ForAll _     -> (0, 1, Array.of_list succ, Some (string_of_state e s))
            | Exists _     -> (0, 0, Array.of_list succ, Some (string_of_state e s))
            | LFP _        -> (0, 0, Array.of_list succ, Some (string_of_state e s))
            | GFP _        -> (0, 0, Array.of_list succ, Some (string_of_state e s)))
        eg


let print_graph g =
    List.iteri (fun i ((e, s), succ) -> print_string ((string_of_int i)^": "^(string_of_state e s)^" "^(String.concat " " (List.map string_of_int succ)))) g


(* replace : 'a -> 'a -> 'a list -> 'a list *)
let rec replace x y = function
    | []                 -> []
    | x'::xs when x' = x -> y :: xs
    | x'::xs             -> x' :: replace x y xs


(* relations : label -> muexpr -> (int * label) list -> (muexpr * int) list *)
let relations l e adj =
    List.fold_left (fun acc (s, l') -> if l = l'
                                       then (e, s)::acc
                                       else acc)
                   []
                   adj


(* make_eg : lts ->
             int list VarMap.t ->
             ((muexpr * int) * int list) list ->
             int ->
             muexpr * int ->
             ((muexpr * int) * int list) list * int *)
let rec make_eg ts var_map g i = function
    | (Var x, s) as node         -> let js = VarMap.find x var_map in
                                    let ((e, s'), _) = List.nth g (List.hd js) in
                                    let js' = List.filter (fun i -> let ((_, s'), _) = List.nth g i in
                                                                    s = s')
                                                          js in
                                    if js' = []
                                    then make_eg ts (VarMap.add x ((i + 1)::js) var_map) (g @ [(node, [i + 1])]) (i + 1) (e, s)
                                    else let j = List.hd js' in
                                         (g @ [(node, [j])], i + 1)
    | (Con (e1, e2), s) as node  -> let (g', i') = make_eg ts var_map (g @ [(node, [i + 1])]) (i + 1) (e1, s) in
                                    make_eg ts var_map (replace (node, [i + 1]) (node, [i + 1; i']) g') i' (e2, s)
    | (Dis (e1, e2), s) as node  -> let (g', i') = make_eg ts var_map (g @ [(node, [i + 1])]) (i + 1) (e1, s) in
                                    make_eg ts var_map (replace (node, [i + 1]) (node, [i + 1; i']) g') i' (e2, s)
    | (ForAll (l, e), s) as node -> let (g', i', succ) = List.fold_left
                                                             (fun (g, i, is) node -> let (g', i') = make_eg ts var_map g i node in
                                                                                     (g', i', i::is))
                                                             (g @ [node, []], i + 1, [])
                                                             (relations l e ts.(s)) in
                                    (replace (node, []) (node, succ) g', i')
    | (Exists (l, e), s) as node -> let (g', i', succ) = List.fold_left
                                                             (fun (g, i, is) node -> let (g', i') = make_eg ts var_map g i node in
                                                                                     (g', i', i::is))
                                                             (g @ [(node, [])], i + 1, [])
                                                             (relations l e ts.(s)) in
                                    (replace (node, []) (node, succ) g', i')
    | (LFP (x, e), s) as node    -> let var_map' = VarMap.add x [i + 1] var_map in
                                    make_eg ts var_map' (g @ [(node, [i + 1])]) (i + 1) (e, s)
    | (GFP (x, e), s) as node    -> let var_map' = VarMap.add x [i + 1] var_map in
                                    make_eg ts var_map' (g @ [(node, [i + 1])]) (i + 1) (e, s)
    | node                       -> (g @ [(node, [])], i + 1)


(* var_prio : int PrioMap.t -> int -> muexpr -> int PrioMap.t *)
let rec var_prio prio_map p = function
    | Neg _         -> raise NegationFailure
    | Con (e1, e2)  -> var_prio (var_prio prio_map p e1) p e2
    | Dis (e1, e2)  -> var_prio (var_prio prio_map p e1) p e2
    | ForAll (_, e) -> var_prio prio_map p e
    | Exists (_, e) -> var_prio prio_map p e
    | LFP (x, e)    -> var_prio (PrioMap.add x (p + 1) prio_map) (p - 10) e
    | GFP (x, e)    -> var_prio (PrioMap.add x p       prio_map) (p - 10) e
    | _             -> prio_map


(* nnf : muexpr -> muexpr *)
let rec nnf = function
    | Neg TT              -> FF
    | Neg FF              -> TT
    | Neg (Var x)         -> raise NegationFailure
    | Neg (Neg e)         -> nnf e
    | Neg (Con (e1, e2))  -> Dis (nnf (Neg e1), nnf (Neg e2))
    | Neg (Dis (e1, e2))  -> Con (nnf (Neg e1), nnf (Neg e2))
    | Neg (ForAll (l, e)) -> Exists (l, nnf (Neg e))
    | Neg (Exists (l, e)) -> ForAll (l, nnf (Neg e))
    | Neg (LFP _)         -> raise NegationFailure
    | Neg (GFP _)         -> raise NegationFailure
    | Con (e1, e2)        -> Con (nnf e1, nnf e2)
    | Dis (e1, e2)        -> Dis (nnf e1, nnf e2)
    | ForAll (l, e)       -> ForAll (l, nnf e)
    | Exists (l, e)       -> Exists (l, nnf e)
    | LFP (x, e)          -> LFP (x, nnf e)
    | GFP (x, e)          -> GFP (x, nnf e)
    | e                   -> e


let print_game game =
    let n = Array.length game in
    print_string ("parity " ^ string_of_int (n-1) ^ ";\n");
    for i = 0 to n - 1 do
      let (pr, pl, delta, desc) = game.(i) in
      if pr >= 0 && pl >= 0 && pl <= 1 then (
            print_int i;
            print_char ' ';
            print_int pr;
            print_char ' ';
            print_int pl;
            print_char ' ';
            for j = 0 to (Array.length delta) - 2 do
              print_string ((string_of_int delta.(j)) ^ ",")
            done;
            if (Array.length delta) > 0 then print_int delta.((Array.length delta) - 1) else ();
            (
             match desc with
               None -> () (* print_string (" \"" ^ string_of_int i ^ "\"") *)
             |   Some s -> if s <> "" then print_string (" \"" ^ s ^ "\"")
            );
            print_char ';';
            print_newline ()
           )
        done


(* make_eg : (lts, int) -> muexpr -> (string list) array -> paritygame *)
let make_pg (ts, s) e v =
    let eval_game = Array.of_list (fst (make_eg ts VarMap.empty [] 0 (nnf e, s))) in
    let prio = var_prio PrioMap.empty 10000 e in
    let parity_game = eg_to_pg eval_game v prio in 
    print_game parity_game


let expr1 = LFP ("x", Dis (Atom "p", ForAll ("a", Var "x")))
let lts1 = [|[(1, "a")]; [(1, "a"); (2, "a")]; [(2, "a")]|]
let v1 = [|[]; []; ["p"]|];;