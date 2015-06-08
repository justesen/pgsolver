type label = string
type variable = string
type lts = (int * label) list array
type valuation = string list array

type muexpr =
    | TT
    | FF
    | Var of variable
    | Neg of muexpr
    | Con of muexpr * muexpr
    | Dis of muexpr * muexpr
    | Exists of label * muexpr
    | ForAll of label * muexpr
    | LFP of string * muexpr
    | GFP of string * muexpr

module StringMap = Map.Make(String)

exception NegationFailure


(* pnf : variable list -> muexpr -> muexpr *)
let rec pnf bv = function
    | Neg TT              -> FF
    | Neg FF              -> TT
    | Neg (Var x)         -> if List.mem x bv
                             then Var x
                             else Neg (Var x)
    | Neg (Neg e)         -> pnf bv e
    | Neg (Con (e1, e2))  -> Dis (pnf bv (Neg e1), pnf bv (Neg e2))
    | Neg (Dis (e1, e2))  -> Con (pnf bv (Neg e1), pnf bv (Neg e2))
    | Neg (Exists (l, e)) -> ForAll (l, pnf bv (Neg e))
    | Neg (ForAll (l, e)) -> Exists (l, pnf bv (Neg e))
    | Neg (LFP (x, e))    -> GFP (x, pnf bv (Neg e))
    | Neg (GFP (x, e))    -> LFP (x, pnf bv (Neg e))
    | Con (e1, e2)        -> Con (pnf bv e1, pnf bv e2)
    | Dis (e1, e2)        -> Dis (pnf bv e1, pnf bv e2)
    | Exists (l, e)       -> Exists (l, pnf bv e)
    | ForAll (l, e)       -> ForAll (l, pnf bv e)
    | LFP (x, e)          -> LFP (x, pnf bv e)
    | GFP (x, e)          -> GFP (x, pnf bv e)
    | e                   -> e


(* bound_vars : muexpr -> variable list *)
let bound_vars e =
    let rec bound_vars' bv = function
        | Neg e         -> bound_vars' bv e
        | Con (e1, e2)
        | Dis (e1, e2)  -> bound_vars' (bound_vars' bv e1) e2
        | Exists (_, e)
        | ForAll (_, e) -> bound_vars' bv e
        | LFP (x, e)
        | GFP (x, e)    -> x :: bound_vars' bv e
        | _             -> bv in
    
    bound_vars' [] e


(* clean : muexpr -> muexpr *)
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
        | Exists (l, e) -> Exists (l, clean' bv e)
        | ForAll (l, e) -> ForAll (l, clean' bv e)
        | LFP (x, e)    -> let x' = fresh x in
                           let bv' = StringMap.add x x' bv in
                           LFP (x', clean' bv' e)
        | GFP (x, e)    -> let x' = fresh x in
                           let bv' = StringMap.add x x' bv in
                           GFP (x', clean' bv' e)
        | e             -> e in
    
    clean' StringMap.empty e
