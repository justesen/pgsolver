let mucalc_to_pg_func lts_file expr_file =
    (* Read LTS file *)
    let lts = ref "" in
    let lts_chan = open_in lts_file in

    try
        while true do
            lts := !lts ^ "\n" ^ input_line lts_chan
        done;
    with End_of_file ->
        close_in lts_chan;

    let (lts, v) = Ltsparse.parse !lts in

    (* Read mu calculus expression *)
    let expr_chan = open_in expr_file in
    let lexbuf = Lexing.from_channel expr_chan in
    let expr = Mucalcparser.main Mucalclexer.token lexbuf in
    close_in expr_chan;

    (* Make parity game *)
    Mucalc.make_pg (lts, 0) expr v;;


let () =
    if Array.length Sys.argv <> 3
    then (print_string ("Mu calculus and Labelled Transition System to parity game\n\n");
          print_string ("Usage: mutopg <lts> <expr>\n\n" ^
                        "       where <lts> is a Labelled Transition System\n" ^
                        "             <expr> is an expression in modal mu calculus\n");
          exit 1)
    else (mucalc_to_pg_func Sys.argv.(1) Sys.argv.(2))
