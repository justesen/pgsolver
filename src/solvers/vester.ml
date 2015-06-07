open Basics
open Paritygame
open Auxiliary
open Auxiliarynf


(* nf_solve : paritygame -> solution * strategy
 * Solve normal form parity game. *)
let rec nf_solve game =
    let l = pg_size game in

    if pg_node_count game = 0 then (
        (Array.make l (-1), Array.make l (-1))
    ) else (
        let u = pg_max_prio_node game in
        let (_, pl, succ, _) = game.(u) in
        let j = 1 - pl in

        (* Solve recursively without u *)
        let game' = pg_copy game in
        pg_remove_nodes game' [u];
        let (win_reg', strat') = nf_solve game' in

        let v = find_v succ win_reg' j in

        if v <> -1 then (
            win_reg'.(u) <- 1 - j;
            strat'.(u) <- v;
            (win_reg', strat')
        ) else (
            let stratA = Array.make l (-1) in
            let attrA = attr game stratA j (u::(win_nodes j win_reg')) in

            (* Solve recursively without A *)
            let game' = pg_copy game in
            pg_remove_nodes game' attrA;
            let (win_reg'', strat'') = nf_solve game' in

            let win_reg = add_to_win_reg l j attrA win_reg'' in
            let strat = merge_strategies l [strat''; stratA; strat'] in
            (win_reg, strat)
        )
    )


(* solve : paritygame -> solution * strategy
 * Solve parity game. *)
let solve game = 
    (* let (win_reg, strat) = nf_solve game in
    (win_reg, sanify_strat game strat win_reg) *)
    let l = pg_size game in
    let nf_game = to_normal_form game in
    let (nf_win_reg, nf_strat) = nf_solve nf_game in
    let win_reg = win_reg_from_nf nf_win_reg l in
    let strat = sanify_strat game (strat_from_nf nf_strat l nf_game) win_reg in
    (win_reg, strat)
    

let _ = Solvers.register_solver
            solve
            "vester"
            "ve"
            "algorithm for normal form parity games"
