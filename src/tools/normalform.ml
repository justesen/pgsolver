open Arg
open Tcsargs


module CommandLine =
struct
    let input_file = ref ""
    let header = Info.get_title "Normal Form Tool"
end


open CommandLine
open Paritygame


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


let _ =
    SimpleArgs.parsedef [] (fun f -> input_file := f) (header ^ "Usage: normalform [infile]\n" ^
                                              "Transforms the parity game given in <infile> to its normal form. If this argument is omitted it reads a game from STDIN.");

    let in_channel = if !input_file = "" then stdin else open_in !input_file in
    let game = ref (parse_parity_game in_channel) in
    print_game (to_normal_form !game)
