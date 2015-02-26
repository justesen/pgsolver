open Paritygame
open Basics


(* is_null : int * int * int array * string option -> bool
 * Is node empty/null? *)
let is_null node =
    node = (-1, -1, [||], None)


(* win_nodes : int -> solution -> int list
 * Nodes from which player j wins. *)
let win_nodes j win_reg =
    snd (Array.fold_left (fun (i, acc) pl -> if pl = j
                                             then (i + 1, i::acc)
                                             else (i + 1, acc))
                         (0, [])
                         win_reg)


(* merge_strategies : strategy list -> strategy
 * Merge strategies into one. *)
let merge_strategies strats =
    let merge s t =
        for i = 0 to (Array.length s) - 1 do
            if s.(i) = -1 then s.(i) <- t.(i)
        done;
        s
    in
    List.fold_left merge (List.hd strats) (List.tl strats)


(* arbitrary_strat : paritygame -> int -> int list -> strategy
 * Create arbitrary strategy for player j on nodes. *)
let arbitrary_strat game j nodes =
    let strat = Array.make (Array.length game) (-1) in
    List.iter (fun v -> let (_, pl, succ, _) = game.(v) in
                        if pl = j && succ <> [||]
                        then strat.(v) <- succ.(0))
              nodes;
    strat


(* solve : paritygame -> solution * strategy
 * Solve parity game. *)
let rec solve game = 
    let l = pg_size game in

    if pg_node_count game = 0 then (
        (Array.make l (-1), Array.make l (-1))
    ) else (
        let j = pg_max_prio game mod 2 in
        let nodes_with_max_prio = collect_max_prio_nodes game in
        let stratA = arbitrary_strat game j nodes_with_max_prio in
        let attrA = attr_closure_inplace game stratA j nodes_with_max_prio in

        (* Solve recursively without nodes that player wins from *)
        let game' = pg_copy game in
        pg_remove_nodes game' attrA;
        let (win_reg, strat) = solve game' in 

        let opp_win_nodes = win_nodes (1 - j) win_reg in

        if opp_win_nodes = [] then (
            let win_reg' = Array.map (fun node -> if is_null node
                                                  then -1
                                                  else j)
                                     game in
            (win_reg', merge_strategies [stratA; strat])
        ) else (
            let stratB = Array.make l (-1) in
            let attrB = attr_closure_inplace game stratB (1 - j) opp_win_nodes in

            (* Solve recursively without nodes that opponent wins from *)
            let game' = pg_copy game in
            pg_remove_nodes game' attrB;
            let (win_reg', strat') = solve game' in

            let win_reg'' = Array.mapi (fun i node -> if is_null node
                                                      then -1
                                                      else if List.exists ((=) i) attrB
                                                      then 1 - j
                                                      else win_reg'.(i))
                                       game in
            (win_reg'', merge_strategies [strat; stratB; strat'])
        )
    )


let _ = Solvers.register_solver
            solve
            "zielonka"
            "zi"
            "my implementation of the recursive algorithm due to Zielonka"
            