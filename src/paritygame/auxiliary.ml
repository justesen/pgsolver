type paritygame = (int * int * int array * string option) array
type solution = int array
type strategy = int array


(* attr : paritygame -> strategy -> int -> int list -> int list
   Synonym for Paritygame.attr_closure_inplace *)
let attr = Paritygame.attr_closure_inplace


(* is_null : int * int * int array * string option -> bool
 * Is node empty/null? *)
let is_null node =
    node = (-1, -1, [||], None)


(* win_nodes : int -> solution -> int list
 * Nodes from which Player j wins. *)
let win_nodes j win_reg =
    let win_nodes = ref [] in
    Array.iteri (fun i pl -> if pl = j then win_nodes := i::!win_nodes)
                win_reg;
    !win_nodes


(* add_to_win_reg : int -> int -> int list -> solution -> solution
   Add nodes as winning for Player j. *)
let add_to_win_reg l j nodes win_reg =
    for i = 0 to l - 1 do
        if List.mem i nodes then win_reg.(i) <- j
    done;
    win_reg


(* merge_strategies : int -> strategy list -> strategy
 * Merge strategies of length l into one. *)
let merge_strategies l strats =
    let merge s t =
        for i = 0 to l - 1 do
            if s.(i) = -1 then s.(i) <- t.(i)
        done;
        s
    in
    List.fold_left merge (Array.make l (-1)) strats


(* sanify_strat : paritygame -> strategy -> solution -> strategy
 * Make sure there is no strategy for Player j, if the node is not in j's
 * winning region. This is necessary in order for the framework to verify the
 * solution (and it makes good sense). *)
let sanify_strat game strat win_reg =
    Array.mapi (fun i v -> let (_, pl, _, _) = game.(i) in
                           if win_reg.(i) <> pl
                           then -1
                           else v)
               strat