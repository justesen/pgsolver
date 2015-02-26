open Paritygame
open Basics


(* is_null : int * int * int array * string option -> bool
 * Is node empty/null? *)
let is_null node =
    node = (-1, -1, [||], None)


(* arbitrary_strat : paritygame -> int -> int list -> strategy *)
let arbitrary_strat game player nodes =
    let strat = Array.make (Array.length game) (-1) in
    List.iter (fun v -> let (_, player', succ, _) = game.(v) in
                        if player' = player && succ <> [||]
                        then strat.(v) <- succ.(0))
              nodes;
    strat


(* win_nodes : int -> solution -> int list
 * Nodes from which player j wins. *)
let win_nodes j win_reg =
    snd (Array.fold_left (fun (i, acc) p -> if p = j
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


(* solve : paritygame -> solution * strategy *)
let rec solve game = 
    let l = pg_size game in

    if pg_node_count game = 0 then (
        (Array.make l (-1), Array.make l (-1))
    ) else (
        let player = pg_max_prio game mod 2 in
        let opponent = 1 - player in
        let nodes_with_max_prio = collect_max_prio_nodes game in
        let stratA = arbitrary_strat game player nodes_with_max_prio in
        let attrA = attr_closure_inplace game stratA player nodes_with_max_prio in

        (* Solve recursively without nodes that player wins from *)
        let game' = pg_copy game in
        pg_remove_nodes game' attrA;
        let (win_reg, strat) = solve game' in 

        let opponent_win_nodes = win_nodes opponent win_reg in

        if opponent_win_nodes = [] then (
            let win_reg' = Array.map (fun node -> if is_null node
                                                  then -1
                                                  else player)
                                     game in
            (win_reg', merge_strategies [stratA; strat])
        ) else (
            let stratB = Array.make l (-1) in
            let attrB = attr_closure_inplace game stratB opponent opponent_win_nodes in

            (* Solve recursively without nodes that opponent wins from *)
            let game' = pg_copy game in
            pg_remove_nodes game' attrB;
            let (win_reg', strat') = solve game' in

            let win_reg'' = Array.mapi (fun i node -> if is_null node
                                                      then -1
                                                      else if List.exists ((=) i) attrB
                                                      then opponent
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