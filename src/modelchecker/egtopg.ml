open Mucalc


type evalgame = ((muexpr * int) * int list) array

exception PositiveNormalFormFailure


(* eg_to_pg : evalgame -> variable list -> valuation -> int StringMap.t -> paritygame *)
let eg_to_pg eg bv v omega p =
    let rec composite = function
        | Con _
        | Dis _ 
        | LFP _
        | GFP _         -> true
        | Exists (_, e)
        | ForAll (_, e) -> composite e
        | _             -> false in

    let rec string_of_muexpr = function
        | TT            -> "TT"
        | FF            -> "FF"
        | Var x         -> x
        | Neg e         -> if composite e
                           then "~("^(string_of_muexpr e)^")"
                           else "~"^(string_of_muexpr e)
        | Con (e1, e2)  -> (if composite e1
                            then "("^(string_of_muexpr e1)^") and "
                            else (string_of_muexpr e1)^" and ")^
                           (if composite e2
                            then "("^(string_of_muexpr e2)^")"
                            else (string_of_muexpr e2))
        | Dis (e1, e2)  -> (if composite e1
                            then "("^(string_of_muexpr e1)^") or "
                            else (string_of_muexpr e1)^" or ")^
                           (if composite e2
                            then "("^(string_of_muexpr e2)^")"
                            else (string_of_muexpr e2))
        | Exists (l, e) -> if composite e
                           then "<"^l^">("^(string_of_muexpr e)^")"
                           else "<"^l^">"^(string_of_muexpr e)
        | ForAll (l, e) -> if composite e
                           then "["^l^"]("^(string_of_muexpr e)^")"
                           else "["^l^"]"^(string_of_muexpr e)
        | LFP (x, e)    -> "mu "^x^"."^(string_of_muexpr e)
        | GFP (x, e)    -> "nu "^x^"."^(string_of_muexpr e) in

    let string_of_state e s =
        "("^(string_of_muexpr e)^", "^(string_of_int s)^")" in

    Array.mapi
        (fun i ((e, s), succ) ->
            match e with
            | TT          -> (p,     1, [|i|], Some (string_of_state e s))
            | FF          -> (p + 1, 0, [|i|], Some (string_of_state e s))
            | Var x       -> if List.mem x bv
                             then (StringMap.find x omega, 0, Array.of_list succ, Some (string_of_state e s))
                             else (if List.mem x v.(s)
                                   then (p,     1, [|i|], Some (string_of_state e s))
                                   else (p + 1, 0, [|i|], Some (string_of_state e s)))
            | Neg (Var x) -> if List.mem x bv
                             then raise PositiveNormalFormFailure
                             else (if List.mem x v.(s)
                                   then (p + 1, 0, [|i|], Some (string_of_state e s))
                                   else (p,     1, [|i|], Some (string_of_state e s)))
            | Neg _       -> raise PositiveNormalFormFailure
            | Con _       -> (0, 1, Array.of_list succ, Some (string_of_state e s))
            | Dis _       -> (0, 0, Array.of_list succ, Some (string_of_state e s))
            | Exists _    -> if succ = []
                             then (p + 1, 0, [|i|], Some (string_of_state e s))
                             else (0,     0, Array.of_list succ, Some (string_of_state e s))
            | ForAll _    -> if succ = []
                             then (p, 1, [|i|], Some (string_of_state e s))
                             else (0, 1, Array.of_list succ, Some (string_of_state e s))
            | LFP _       -> (0, 0, Array.of_list succ, Some (string_of_state e s))
            | GFP _       -> (0, 0, Array.of_list succ, Some (string_of_state e s)))
        eg


(* var_prio : muexpr -> int StringMap.t *)
let var_prio e =
    let rec var_prio' prio_map p = function
        | Con (e1, e2)
        | Dis (e1, e2)  -> var_prio' (var_prio' prio_map p e1) p e2
        | Exists (_, e)
        | ForAll (_, e) -> var_prio' prio_map p e
        | LFP (x, e)    -> var_prio' (StringMap.add x (p + 1) prio_map) (p - 2) e
        | GFP (x, e)    -> var_prio' (StringMap.add x p       prio_map) (p - 2) e
        | _             -> prio_map in
    
    var_prio' StringMap.empty e


(* max_prio : muexpr -> int *)
let max_prio e =
    let rec max_prio' p = function
        | Con (e1, e2)
        | Dis (e1, e2)  -> max_prio' (max_prio' p e1) e2
        | Exists (_, e)
        | ForAll (_, e) -> max_prio' p e
        | LFP (x, e)
        | GFP (x, e)    -> max_prio' (p + 2) e
        | _             -> p in
    max_prio' 2 e


(* make_eg : lts -> variable list -> muexpr * int -> evalgame *)
let make_eg ts bv (e, s) =
    let relations l e adj =
        List.fold_left (fun acc (s, l') -> if l = l' || l = ""
                                           then (e, s)::acc
                                           else acc)
                       []
                       adj in

    let rec make_eg' var_map i = function
        | (Var x, s) as node when List.mem x bv
                                     -> let is = StringMap.find x var_map in
                                        (try
                                            let (i', _, _) = List.find (fun (_, _, s') -> s = s) is in
                                            ([(node, [i'])], var_map, i + 1)
                                         with
                                            Not_found -> let (_, e, _) = (List.hd is) in
                                                         let vm = StringMap.add x ((i + 1, e, s)::is) var_map in
                                                         let (g', var_map', i') = make_eg' vm (i + 1) (e, s) in
                                                         ((node, [i + 1])::g', var_map', i'))
        | (Con (e1, e2), s)
        | (Dis (e1, e2), s) as node  -> let (g', var_map', i') = make_eg' var_map (i + 1) (e1, s) in
                                        let (g'', var_map'', i'') = make_eg' var_map' i' (e2, s) in
                                        ((node, [i + 1; i'])::(g' @ g''), var_map'', i'')
        | (Exists (l, e), s)
        | (ForAll (l, e), s) as node -> let (g', var_map', i', succ) =
                                            List.fold_left (fun (g, vm, i, is) node -> let (g', vm', i') = make_eg' vm i node in
                                                                                       (g @ g', vm', i', i::is))
                                                           ([], var_map, i + 1, [])
                                                           (relations l e ts.(s)) in
                                        ((node, succ)::g', var_map', i')
        | (LFP (x, e), s)
        | (GFP (x, e), s) as node    -> let var_map' = StringMap.add x [(i + 1, e, s)] var_map in
                                        let (g, var_map'', i') = make_eg' var_map' (i + 1) (e, s) in
                                        ((node, [i + 1])::g, var_map'', i')
        | node                       -> ([(node, [])], var_map, i + 1) in

    let (eg, _, _) = make_eg' StringMap.empty 0 (e, s) in
    Array.of_list eg


(* make_eg_iter : lts -> variable list -> muexpr * int -> evalgame *)
let make_eg_iter ts bv (e, s) =
    let relations l e adj =
        List.fold_left (fun acc (s, l') -> if l = l' || l = ""
                                           then (e, s)::acc
                                           else acc)
                       []
                       adj in

    (* Create graph of evaluation game, iteratively, by using two stacks for
       a post-order tree traversal. *)
    let pre_st = Stack.create () in
    let post_st = Stack.create () in
    let var_map = ref StringMap.empty in
    let i = ref 0 in

    Stack.push (e, s) pre_st;

    while not (Stack.is_empty pre_st) do
        let current = Stack.pop pre_st in

        (match current with
         | (Var x, s) as node when List.mem x bv
                                      -> let is = StringMap.find x !var_map in
                                         (try
                                             let (i', _, _) = List.find (fun (_, _, s') -> s = s') is in
                                             Stack.push (node, [i']) post_st
                                          with
                                             Not_found -> let (_, e, _) = List.hd is in
                                                          var_map := StringMap.add x ((!i + 1, e, s)::is) !var_map;
                                                          Stack.push (node, [!i + 1]) post_st;
                                                          Stack.push (e, s) pre_st)
         | (Con (e1, e2), s)
         | (Dis (e1, e2), s) as node  -> Stack.push (node, []) post_st;
                                         Stack.push (e2, s) pre_st;
                                         Stack.push (e1, s) pre_st
         | (Exists (l, e), s)
         | (ForAll (l, e), s) as node -> Stack.push (node, []) post_st;
                                         List.iter (fun (e', s') -> Stack.push (e', s') pre_st) (List.rev (relations l e ts.(s)))
         | (LFP (x, e), s)
         | (GFP (x, e), s) as node    -> var_map := StringMap.add x [(!i + 1, e, s)] !var_map;
                                         Stack.push (node, [!i + 1]) post_st;
                                         Stack.push (e, s) pre_st
         | node                       -> Stack.push (node, []) post_st);

        i := !i + 1
    done;

    let g = ref [] in
    Stack.iter (fun (node, succ) -> g := (node, succ)::!g) post_st;
    let eg = Array.of_list !g in

    (* Find the indices of successors that are missing in the game graph. *)
    Array.mapi
        (fun i (node, succ) ->
            match node with
            | (Con (e1, e2), s)
            | (Dis (e1, e2), s)  -> let rec find_succ j =
                                        let ((e', s'), _) = eg.(j) in
                                        if (e', s') = (e2, s)
                                        then j
                                        else find_succ (j + 1) in

                                    (node, [i + 1; find_succ (i + 1)])
            | (Exists (l, e), s)
            | (ForAll (l, e), s) -> let rec find_succ j succ' = function
                                    | []      -> succ'
                                    | nb::nbs -> let (node', _) = eg.(j) in
                                                 if node' = nb
                                                 then find_succ (j + 1) (j::succ') nbs
                                                 else find_succ (j + 1) succ' (nb::nbs) in

                                    (node, List.rev (find_succ (i + 1) [] (relations l e ts.(s))))
            | _                  -> (node, succ))
        eg


(* make_pg : (lts, int) -> muexpr -> valuation -> paritygame *)
let make_pg (ts, s) e v =
    let e' = clean e in
    let bv = bound_vars e' in
    let xi = pnf bv e' in
    let eg = make_eg_iter ts bv (xi, s) in
    let p = max_prio xi in
    let omega = var_prio p xi in
    eg_to_pg eg bv v omega p
