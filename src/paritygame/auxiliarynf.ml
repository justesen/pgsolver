open Paritygame


type paritygame = (int * int * int array * string option) array
type solution = int array
type strategy = int array


(* find_v : int array -> int array -> int -> int
 * Find a node v in succ, such that v is in the winning region of Player
 * 1 - j. *)
let find_v succ win_reg j =
    Array.fold_left (fun acc v -> if win_reg.(v) = 1 - j
                                  then v
                                  else acc)
                    (-1)
                    succ


(* win_reg_from_nf : solution -> int -> solution
 * Get winning regions from normal form, i.e., to original form of input. *)
let win_reg_from_nf win_reg l =
    Array.sub win_reg 0 l


(* strat_from_nf : solution -> int -> paritygame -> solution
 * Get strategy from normal form, i.e., to original form of input. *)
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