let random_nf_game_func arguments =

    let show_help _ =
        print_string (Info.get_title "Random Normal Form Game Generator");
        print_string ("Usage: randomnfgame n p l h\n\n" ^
                      "       where n = Number of nodes\n" ^
                      "             p = Highest possibly occurring priority\n" ^
                      "             l = Lowest possible out-degree\n" ^
                      "             h = Highest possible out-degree\n\n")
    in
    
    if (Array.length arguments <> 4) then (show_help (); exit 1);

    let size = int_of_string arguments.(0) in
    let max_prio = 1+(int_of_string arguments.(1)) in
    let outdegmin = int_of_string arguments.(2) in
    let outdegmax = int_of_string arguments.(3) in

    Random.self_init ();

    let rec get_rand_parity parity max =
      let x = Random.int max in
      if x mod 2 = parity
      then x
      else (if x + 1 > max then (get_rand_parity parity max) else (x + 1))
    in

    let pg = Array.init size (fun i -> (get_rand_parity (i mod 2) max_prio,
                                        i mod 2,
                                        Array.of_list (Array.fold_left
                                                       (fun acc j -> let v = if (i mod 2) = (j mod 2) then (j + 1) else j in
                                                                     if List.exists ((=) v) acc then acc else v::acc)
                                                       []
                                                       (Tcsmaths.RandomUtils.get_pairwise_different_from_range (outdegmin + Random.int (outdegmax - outdegmin + 1))
                                                                                                               0
                                                                                                               (size - 2))),
                                        Some (string_of_int i))) in
    pg;;
    
Generators.register_generator random_nf_game_func "randomnfgame" "Random Normal Form Game";;