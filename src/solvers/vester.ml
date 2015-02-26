open Basics
open Paritygame


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


(* find_w : int array -> int list -> int -> int
 * Find a node w in succ, such that w is in the winning region of player
 * 1 - j. *)
let find_w succ win_reg j =
    Array.fold_left (fun acc w -> if win_reg.(w) = 1 - j
                                  then w
                                  else acc)
                    (-1)
                    succ


(* nf_solve : paritygame -> solution * strategy
 * Solve normal form parity game. *)
let rec nf_solve game =
    let l = pg_size game in
    let n = pg_max_prio_node game in
    let (_, pl, succ, _) = game.(n) in
    let j = 1 - pl in

    (* message 3 (fun () -> game_to_string game); *)
    (* message 3 (fun () -> string_of_int (pg_node_count game)); *)
    (* message 3 (fun () -> "l="^(string_of_int l)^"   n="^(string_of_int n)^"   j="^(string_of_int j)^"\n"); *)
    (* Scanf.bscanf Scanf.Scanning.stdin " %d" (fun x -> x); *)

    if pg_node_count game = 0 then (
        (Array.make l (-1), Array.make l (-1))
    ) else (
        (* Solve recursively without node n *)
        let game' = pg_copy game in
        pg_remove_nodes game' [n];
        let (win_reg, strat) = nf_solve game' in

        let w = find_w succ win_reg j in

        if w <> -1 then (
            win_reg.(n) <- 1 - j;
            strat.(n) <- w;
            (win_reg, strat)
        ) else (
            let attrA = attr_closure_inplace game strat j (n::(win_nodes j win_reg)) in

            (* Solve recursively without nodes that player j wins from *)
            let game' = pg_copy game in
            pg_remove_nodes game' attrA;
            let (win_reg', strat') = nf_solve game' in

            let win_reg'' = Array.mapi (fun i node -> if is_null node
                                                      then -1
                                                      else if List.exists ((=) i) attrA
                                                      then j
                                                      else win_reg'.(i))
                                       game in
            (win_reg'', merge_strategies [strat'; strat])
        )
    )


(* sanify_strat : paritygame -> strategy -> solution -> strategy
 * Make sure there is no strategy for player j if the node is not in j's
 * winning region. This is necessary in order for the framework to verify the
 * solution. *)
let sanify_strat game strat win_reg =
    Array.mapi (fun i v -> let (_, pl, _, _) = game.(i) in
                           if win_reg.(i) <> pl
                           then -1
                           else v)
               strat


(* win_reg_from_nf : solution -> int -> solution
 * Get winning regions from normal form, i.e. to original form of input. *)
let win_reg_from_nf win_reg l =
    Array.sub win_reg 0 l


(* strat_from_nf : solution -> int -> solution
 * Get strategy from normal form, i.e. to original form of input. *)
let strat_from_nf strat l nf_game =
    Array.mapi (fun i v -> if (pg_get_tr nf_game i).(0) = i + 2*l
                           then let w = i + 2*l in
                                if strat.(w) < l then strat.(w) else strat.(w) - l
                           else if v < l then v else v - l)
               (Array.sub strat 0 l)


(* to_normal_form : paritygame -> paritygame
 * Transform parity game to normal form. *)
let to_normal_form game =
    let l = pg_size game in
    let nf_game = Array.make (3*l) (-1, -1, [||], None) in

    Array.iteri (fun i (pr, pl, succ, label) -> 
                    if pr mod 2 = pl then (
                        nf_game.(i + l) <- (1 - pl, 1 - pl, [|i|], None);
                        nf_game.(i)     <- (pr + 2, pl,     succ,  label)
                    ) else (
                        nf_game.(i + l)   <- (pl,     pl,     [|i|],       None);
                        nf_game.(i)       <- (pr + 2, 1 - pl, [|i + 2*l|], label);
                        nf_game.(i + 2*l) <- (pl,     pl,     succ,        None)
                    )
                )
                game;

    for i = 0 to 3 * l - 1 do
        let (pr, pl, succ, label) = nf_game.(i) in
        let succ' = Array.map (fun v -> if pg_get_pl nf_game v = pl
                                        then v + l
                                        else v)
                              succ in
        nf_game.(i) <- (pr, pl, succ', label)
    done;

    nf_game


(* solve : paritygame -> solution * strategy
 * Solve parity game. *)
let solve game = 
    let (win_reg, strat) = nf_solve game in
    (win_reg, sanify_strat game strat win_reg)
    (* let l = pg_size game in
    let nf_game = to_normal_form game in
    let (nf_win_reg, nf_strat) = nf_solve nf_game in
    let win_reg = win_reg_from_nf nf_win_reg l in
    let strat = sanify_strat game (strat_from_nf nf_strat l nf_game) win_reg in
    (win_reg, strat) *)
    

let _ = Solvers.register_solver
            solve
            "vester"
            "ve"
            "algorithm for normal form parity games"
