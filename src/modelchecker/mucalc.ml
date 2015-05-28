type label = string

type lts = (int * label) list array

type valuation = string list array

type muexpr =
    | TT
    | FF
    | Var of string
    | Neg of muexpr
    | Con of muexpr * muexpr
    | Dis of muexpr * muexpr
    | ForAll of label * muexpr
    | Exists of label * muexpr
    | LFP of string * muexpr
    | GFP of string * muexpr

type evalgame = ((muexpr * int) * int list) array

module StringMap = Map.Make(String)

exception NegationFailure


(* print_game : paritygame -> unit *)
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
            (match desc with
             | None   -> ()
             | Some s -> if s <> "" then print_string (" \"" ^ s ^ "\""));
            print_char ';';
            print_newline ()
        )
    done


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
    | Var x          -> x
    | Neg e          -> if composite e
                        then "~("^(string_of_muexpr e)^")"
                        else "~"^(string_of_muexpr e)
    | Con (e1, e2)   -> (if composite e1
                         then "("^(string_of_muexpr e1)^") and "
                         else (string_of_muexpr e1)^" and ")^
                        (if composite e2
                         then "("^(string_of_muexpr e2)^")"
                         else (string_of_muexpr e2))
    | Dis (e1, e2)   -> (if composite e1
                         then "("^(string_of_muexpr e1)^") or "
                         else (string_of_muexpr e1)^" or ")^
                        (if composite e2
                         then "("^(string_of_muexpr e2)^")"
                         else (string_of_muexpr e2))
    | ForAll (l, e)  -> if composite e
                        then "["^l^"] ("^(string_of_muexpr e)^")"
                        else "["^l^"] "^(string_of_muexpr e)
    | Exists (l, e)  -> if composite e
                        then "<"^l^"> ("^(string_of_muexpr e)^")"
                        else "<"^l^"> "^(string_of_muexpr e)
    | LFP (x, e)     -> "mu "^x^"."^(string_of_muexpr e)
    | GFP (x, e)     -> "nu "^x^"."^(string_of_muexpr e)


(* string_of_muexpr : muexpr -> int -> string *)
let string_of_state e s =
    "("^(string_of_muexpr e)^", "^(string_of_int s)^")"


(* eg_to_pg : evalgame ->
              string list ->
              valuation ->
              int StringMap.t ->
              int ->
              paritygame *)
let eg_to_pg eg bv v prio p_max =
    Array.mapi
        (fun i ((e, s), succ) ->
            match e with
            | TT           -> (p_max, 1, Array.of_list (i::succ), Some (string_of_state e s))
            | FF           -> (p_max + 1, 0, Array.of_list (i::succ), Some (string_of_state e s))
            | Var x        -> if List.mem x bv
                              then (StringMap.find x prio, 0, Array.of_list succ, Some (string_of_state e s))
                              else (if List.mem x v.(s)
                                    then (p_max,     1, Array.of_list (i::succ), Some (string_of_state e s))
                                    else (p_max + 1, 0, Array.of_list (i::succ), Some (string_of_state e s)))
            | Neg (Var x)  -> if List.mem x bv
                              then raise NegationFailure
                              else (if List.mem x v.(s)
                                    then (p_max + 1, 0, Array.of_list (i::succ), Some (string_of_state e s))
                                    else (p_max,     1, Array.of_list (i::succ), Some (string_of_state e s)))
            | Neg _        -> raise NegationFailure
            | Con _        -> (0, 1, Array.of_list succ, Some (string_of_state e s))
            | Dis _        -> (0, 0, Array.of_list succ, Some (string_of_state e s))
            | ForAll _     -> if succ = []
                              then (p_max, 1, [|i|], Some (string_of_state e s))
                              else (0,     1, Array.of_list succ, Some (string_of_state e s))
            | Exists _     -> if succ = []
                              then (p_max + 1, 0, [|i|], Some (string_of_state e s))
                              else (0,         0, Array.of_list succ, Some (string_of_state e s))
            | LFP _        -> (0, 0, Array.of_list succ, Some (string_of_state e s))
            | GFP _        -> (0, 0, Array.of_list succ, Some (string_of_state e s)))
        eg


(* replace : 'a -> 'a -> 'a list -> 'a list *)
let rec replace x y = function
    | []                 -> []
    | x'::xs when x' = x -> y :: xs
    | x'::xs             -> x' :: replace x y xs


(* relations : label -> muexpr -> (int * label) list -> (muexpr * int) list *)
let relations l e adj =
    List.fold_left (fun acc (s, l') -> if l = l' || l = ""
                                       then (e, s)::acc
                                       else acc)
                   []
                   adj


(* make_eg' : lts ->
              int list StringMap.t ->
              ((muexpr * int) * int list) array ->
              int ->
              muexpr * int ->
              ((muexpr * int) * int list) array * int list StringMap.t * int *)
let rec make_eg' ts var_map g bv i = function
    | (Var x, s) as node when List.mem x bv
                                 -> let is = StringMap.find x var_map in
                                    (try
                                        let i' = List.find (fun i -> let ((_, s'), _) = List.nth g i in s = s') is in
                                        (g @ [(node, [i'])], var_map, i + 1)
                                     with
                                        Not_found -> let ((e, _), _) = List.nth g (List.hd is) in
                                                     make_eg' ts (StringMap.add x ((i + 1)::is) var_map) (g @ [(node, [i + 1])]) bv (i + 1) (e, s))
    | (Con (e1, e2), s) as node  -> let (g', var_map', i') = make_eg' ts var_map (g @ [(node, [i + 1])]) bv (i + 1) (e1, s) in
                                    make_eg' ts var_map' (replace (node, [i + 1]) (node, [i + 1; i']) g') bv i' (e2, s)
    | (Dis (e1, e2), s) as node  -> let (g', var_map', i') = make_eg' ts var_map (g @ [(node, [i + 1])]) bv (i + 1) (e1, s) in
                                    make_eg' ts var_map' (replace (node, [i + 1]) (node, [i + 1; i']) g') bv i' (e2, s)
    | (ForAll (l, e), s) as node -> let (g', var_map', i', succ) =
                                        List.fold_left (fun (g, vm, i, is) node -> let (g', vm', i') = make_eg' ts vm g bv i node in
                                                                                   (g', vm', i', i::is))
                                                       (g @ [(node, [])], var_map, i + 1, [])
                                                       (relations l e ts.(s)) in
                                    (replace (node, []) (node, succ) g', var_map', i')
    | (Exists (l, e), s) as node -> let (g', var_map', i', succ) =
                                        List.fold_left (fun (g, vm, i, is) node -> let (g', vm', i') = make_eg' ts vm g bv i node in
                                                                                   (g', vm', i', i::is))
                                                       (g @ [(node, [])], var_map, i + 1, [])
                                                       (relations l e ts.(s)) in
                                    (replace (node, []) (node, succ) g', var_map', i')
    | (LFP (x, e), s) as node    -> let var_map' = StringMap.add x [i + 1] var_map in
                                    make_eg' ts var_map' (g @ [(node, [i + 1])]) bv (i + 1) (e, s)
    | (GFP (x, e), s) as node    -> let var_map' = StringMap.add x [i + 1] var_map in
                                    make_eg' ts var_map' (g @ [(node, [i + 1])]) bv (i + 1) (e, s)
    | node                       -> (g @ [(node, [])], var_map, i + 1)


(* make_eg : lts -> string list -> muexpr * int *)
let make_eg ts bv (e, s) =
    let (eg, _, _) = make_eg' ts StringMap.empty [] bv 0 (e, s) in
    Array.of_list eg   


(* var_prio : muexpr -> int StringMap.t *)
let var_prio e =
    let rec var_prio' prio_map p = function
        | Con (e1, e2)  -> var_prio' (var_prio' prio_map p e1) p e2
        | Dis (e1, e2)  -> var_prio' (var_prio' prio_map p e1) p e2
        | ForAll (_, e) -> var_prio' prio_map p e
        | Exists (_, e) -> var_prio' prio_map p e
        | LFP (x, e)    -> var_prio' (StringMap.add x (p + 1) prio_map) (p - 2) e
        | GFP (x, e)    -> var_prio' (StringMap.add x p       prio_map) (p - 2) e
        | _             -> prio_map
    in var_prio' StringMap.empty e


(* max_prio : muexpr -> int *)
let max_prio e =
    let rec max_prio' p = function
        | Con (e1, e2)  -> max_prio' (max_prio' p e1) e2
        | Dis (e1, e2)  -> max_prio' (max_prio' p e1) e2
        | ForAll (_, e) -> max_prio' p e
        | Exists (_, e) -> max_prio' p e
        | LFP (x, e)    -> max_prio' (p + 2) e
        | GFP (x, e)    -> max_prio' (p + 2) e
        | _             -> p
    in max_prio' 2 e


(* pnf : muexpr -> string list -> muexpr *)
let rec pnf bv = function
    | Neg TT              -> FF
    | Neg FF              -> TT
    | Neg (Var x)         -> if List.mem x bv
                             then Var x
                             else Neg (Var x)
    | Neg (Neg e)         -> pnf bv e
    | Neg (Con (e1, e2))  -> Dis (pnf bv (Neg e1), pnf bv (Neg e2))
    | Neg (Dis (e1, e2))  -> Con (pnf bv (Neg e1), pnf bv (Neg e2))
    | Neg (ForAll (l, e)) -> Exists (l, pnf bv (Neg e))
    | Neg (Exists (l, e)) -> ForAll (l, pnf bv (Neg e))
    | Neg (LFP (x, e))    -> GFP (x, pnf bv (Neg e))
    | Neg (GFP (x, e))    -> LFP (x, pnf bv (Neg e))
    | Con (e1, e2)        -> Con (pnf bv e1, pnf bv e2)
    | Dis (e1, e2)        -> Dis (pnf bv e1, pnf bv e2)
    | ForAll (l, e)       -> ForAll (l, pnf bv e)
    | Exists (l, e)       -> Exists (l, pnf bv e)
    | LFP (x, e)          -> LFP (x, pnf bv e)
    | GFP (x, e)          -> GFP (x, pnf bv e)
    | e                   -> e


(* bound_vars : muexpr -> string list *)
let bound_vars e =
    let rec bound_vars' bv = function
        | Neg e          -> bound_vars' bv e
        | Con (e1, e2)   -> bound_vars' (bound_vars' bv e1) e2
        | Dis (e1, e2)   -> bound_vars' (bound_vars' bv e1) e2
        | ForAll (_, e)  -> bound_vars' bv e
        | Exists (_, e)  -> bound_vars' bv e
        | LFP (x, e)     -> x :: bound_vars' bv e
        | GFP (x, e)     -> x :: bound_vars' bv e
        | _              -> bv
    in bound_vars' [] e


(* clean : string StringMap.t -> muexpr -> muexpr *)
let clean e =
    let var_no = ref 0 in

    let fresh var = var_no := !var_no + 1;
                    var^(string_of_int !var_no) in

    let rec clean' bv = function
        | Var x         -> if StringMap.mem x bv
                           then Var (StringMap.find x bv)
                           else Var x
        | Neg e         -> Neg (clean' bv e)
        | Con (e1, e2)  -> Con (clean' bv e1, clean' bv e2)
        | Dis (e1, e2)  -> Dis (clean' bv e1, clean' bv e2)
        | ForAll (l, e) -> ForAll (l, clean' bv e)
        | Exists (l, e) -> Exists (l, clean' bv e)
        | LFP (x, e)    -> let x' = fresh x in
                           let bv' = StringMap.add x x' bv in
                           LFP (x', clean' bv' e)
        | GFP (x, e)    -> let x' = fresh x in
                           let bv' = StringMap.add x x' bv in
                           GFP (x', clean' bv' e)
        | e             -> e
    in clean' StringMap.empty e


(* make_eg : (lts, int) -> muexpr -> valuation -> unit *)
let make_pg (ts, s) e v =
    let e' = clean e in
    let bv = bound_vars e' in
    let formula = pnf bv e' in
    let eval_game = make_eg ts bv (formula, s) in
    let p_max = max_prio formula in
    let prio_map = var_prio p_max formula in
    let parity_game = eg_to_pg eval_game bv v prio_map p_max in 
    parity_game
    (* print_game parity_game *)
