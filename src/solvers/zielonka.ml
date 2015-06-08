open Paritygame
open Basics
open Auxiliary


(* arbitrary_strat : paritygame -> int -> int list -> strategy
 * Create arbitrary strategy for Player j on nodes. *)
let arbitrary_strat game j nodes =
    let strat = Array.make (Array.length game) (-1) in
    List.iter (fun v -> let (_, pl, succ, _) = game.(v) in
                        if pl = j && succ <> [||]
                        then strat.(v) <- succ.(0))
              nodes;
    strat


(* zsolve : paritygame -> solution * strategy
 * Solve parity game. *)
let rec zsolve game = 
    let l = pg_size game in

    if pg_node_count game = 0 then (
        (Array.make l (-1), Array.make l (-1))
    ) else (
        let j = pg_max_prio game mod 2 in
        let setU = collect_max_prio_nodes game in
        let stratA = arbitrary_strat game j setU in
        let attrA = attr game stratA j setU in

        (* Solve recursively without A *)
        let game' = pg_copy game in
        pg_remove_nodes game' attrA;
        let (win_reg', strat') = zsolve game' in 

        let opp_win_nodes = win_nodes (1 - j) win_reg' in

        if opp_win_nodes = [] then (
            let win_reg = Array.map (fun node -> if is_null node
                                                 then -1
                                                 else j)
                                     game in
            (win_reg, merge_strategies l [strat'; stratA])
        ) else (
            let stratB = Array.make l (-1) in
            let attrB = attr game stratB (1 - j) opp_win_nodes in

            (* Solve recursively without B *)
            let game' = pg_copy game in
            pg_remove_nodes game' attrB;
            let (win_reg'', strat'') = zsolve game' in

            let win_reg = add_to_win_reg l (1 - j) attrB win_reg'' in
            let strat = merge_strategies l [strat''; stratB; strat'] in
            (win_reg, strat)
        )
    )


(* solve : paritygame -> solution * strategy
 * Solve parity game using Zielonka's Algorithm and sanify the strategy. *)
let solve game =
  let (win_reg, strat) = zsolve game in
  (win_reg, sanify_strat game strat win_reg)


let _ = Solvers.register_solver
            solve
            "zielonka"
            "zi"
            "my implementation of the recursive algorithm due to Zielonka"
