open Basics
open Paritygame
open Auxiliary
open Auxiliarynf


(* find_T : paritygame -> int list -> int array -> int -> int list * strategy
 * Find the set T of nodes that have a successor v in win_reg, and find this
 * strategy from nodes u in T to v. *)
let find_T game setU win_reg j =
    let setT = ref [] in
    let stratT = Array.make (pg_size game) (-1) in
    List.iter (fun u -> let (_, _, succ, _) = game.(u) in
                        stratT.(u) <- find_v succ win_reg j;
                        if stratT.(u) <> -1
                        then setT := u::!setT)
              setU;
    (!setT, stratT)


(* nf_solve : paritygame -> solution * strategy
 * Solve normal form parity game. *)
let rec nf_solve game =
    let l = pg_size game in 

    if pg_node_count game = 0 then (
        (Array.make l (-1), Array.make l (-1))
    ) else (
        let setU = collect_max_prio_nodes game in
        let (_, pl, _, _) = game.(List.hd setU) in
        let j = 1 - pl in

        (* Solve recursively without U *)
        let game' = pg_copy game in
        pg_remove_nodes game' setU;
        let (win_reg', strat') = nf_solve game' in

        let (setT, stratT) = find_T game setU win_reg' j in

        if List.length setT = List.length setU then (
            let win_reg = add_to_win_reg l (1 - j) setT win_reg' in
            let strat = merge_strategies l [strat'; stratT] in
            (win_reg, strat)
        ) else (
            let setU_T = List.filter (fun u -> not (List.mem u setT)) setU in
            let stratA = Array.make l (-1) in
            let attrA = attr game stratA j (setU_T @ (win_nodes j win_reg')) in

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
            "vester2"
            "ve2"
            "improved algorithm for normal form parity games"
