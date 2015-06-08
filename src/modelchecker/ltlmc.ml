(* print_tree : paritygame -> strategy -> unit *)
let print_tree pg strat =
    print_string "Tree:";

    let rec print_tree' visited indent i =
        if List.mem i visited
        then (print_string "\n")
        else (print_string ("\n"^indent^(Paritygame.pg_get_desc' pg i));
              if strat.(i) = -1
              then let succ = Paritygame.pg_get_tr pg i in
                   Array.iter (print_tree' (i::visited)
                                            (if Array.length succ = 1
                                             then indent
                                             else indent^"    "))
                              (Paritygame.pg_get_tr pg i)
              else print_tree' (i::visited) indent strat.(i)) in

    print_tree' [] "    " 0


(* model_checker : string -> string -> unit *)
let model_checker lts_file expr_file =
    (* Read LTS file *)
    let lts = ref "" in
    let lts_chan = open_in lts_file in

    try
        while true do
            lts := !lts ^ "\n" ^ input_line lts_chan
        done;
    with End_of_file -> close_in lts_chan;

    let (lts, v) = Ltsparser.parse !lts in

    (* Read LTL expression *)
    let expr_chan = open_in expr_file in
    let lexbuf = Lexing.from_channel expr_chan in
    let expr = Ltlparser.main Ltllexer.token lexbuf in
    close_in expr_chan;

    (* Solve parity game *)
    let pg = Egtopg.make_pg (lts, 0) (Ltl.to_mucalc expr) v in
    let (win_reg, strat) = Zielonka.solve pg in

    (* Output *)
    if win_reg.(0) = 0
    then print_string "Yes\n\n"
    else print_string "No\n\n";
    print_tree pg strat


let () =
    if Array.length Sys.argv <> 3
    then (print_string ("Model checking of Labelled Transition System and a property in Linear Temporal Logic\n\n");
          print_string ("Usage: ltlmc <lts> <expr>\n\n" ^
                        "       where <lts> is a Labelled Transition System\n" ^
                        "             <expr> is an expression in LTL\n");
          exit 1)
    else (model_checker Sys.argv.(1) Sys.argv.(2))
