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


exception NegationFailure


let rec range i j =
    if i >= j
    then []
    else i :: range (i + 1) j


(* relations : label -> muexpr -> (int * label) list -> (muexpr * int) list *)
let relations l e adj =
    List.fold_left (fun acc (s, l') -> if l = l'
                                       then (e, s)::acc
                                       else acc)
                   []
                   adj


(* nnf : muexpr -> muexpr *)
let rec nnf = function
    | Neg TT              -> FF
    | Neg FF              -> TT
    | Neg (Atom a)        -> Neg (Atom a)
    | Neg (Var x)         -> raise NegationFailure
    | Neg (Neg e)         -> nnf e
    | Neg (Con (e1, e2))  -> Dis (nnf (Neg e1), nnf (Neg e2))
    | Neg (Dis (e1, e2))  -> Con (nnf (Neg e1), nnf (Neg e2))
    | Neg (ForAll (l, e)) -> ForAll (l, nnf (Neg e))
    | Neg (Exists (l, e)) -> Exists (l, nnf (Neg e))
    | Neg (LFP _)         -> raise NegationFailure
    | Neg (GFP _)         -> raise NegationFailure
    | Con (e1, e2)        -> Con (nnf e1, nnf e2)
    | Dis (e1, e2)        -> Dis (nnf e1, nnf e2)
    | ForAll (l, e)       -> ForAll (l, nnf e)
    | Exists (l, e)       -> Exists (l, nnf e)
    | LFP (x, e)          -> LFP (x, nnf e)
    | GFP (x, e)          -> GFP (x, nnf e)
    | e                   -> e


let rec make_graph ts var_map g i = function
    | (Var x, s) as node         -> (g @ [(node, [VarMap.find x var_map])], i + 1)
    | (Con (e1, e2), s) as node  -> let (g1, i1) = make_graph ts var_map [] (i + 1) (e1, s) in
                                    let (g2, i2) = make_graph ts var_map [] i1      (e2, s) in
                                    (g @ [(node, [i1; i2])] @ g1 @ g2, i2)
    | (Dis (e1, e2), s) as node  -> let (g1, i1) = make_graph ts var_map [] (i + 1) (e1, s) in
                                    let (g2, i2) = make_graph ts var_map [] i1      (e2, s) in
                                    (g @ [(node, [i1; i2])] @ g1 @ g2, i2)
    | (ForAll (l, e), s) as node -> let (g', i') = List.fold_left
                                                       (fun (g, i) node -> let (g', i') = make_graph ts var_map [] i node in
                                                                           (g @ g', i'))
                                                       ([], i + 1)
                                                       (relations l e ts.(s)) in
                                   (g @ [node, range (i + 1) i'] @ g', i' + 1)
    | (Exists (l, e), s) as node -> let (g', i') = List.fold_left
                                                       (fun (g, i) node -> let (g', i') = make_graph ts var_map [] i node in
                                                                           (g @ g', i'))
                                                       ([], i + 1)
                                                       (relations l e ts.(s)) in
                                   (g @ [node, range (i + 1) i'] @ g', i' + 1)
    | (LFP (x, e), s) as node    -> let var_map' = VarMap.add x i var_map in
                                    let (g', i') = make_graph ts var_map' [] (i + 1) (e, s) in
                                    (g @ [(node, [i + 1])] @ g', i')
    | (GFP (x, e), s) as node    -> let var_map' = VarMap.add x i var_map in
                                    let (g', i') = make_graph ts var_map' [] (i + 1) (e, s) in
                                    (g @ [(node, [i + 1])] @ g', i')
    | node                       -> (g @ [(node, [])], i + 1)


(* make_graph : (lts, int) -> muexpr -> string array -> paritygame *)
let make_pg (ts, s) e p =
    let eval_game = fst (make_graph ts (VarMap.empty) [] 0 (nnf e, s)) in
    eval_game


let expr1 = LFP ("x", Dis (Atom "p", ForAll ("a", Var "x")))
let lts1 = [|[(1, "a")]; [(1, "a"); (2, "a")]; [(2, "a")]|]