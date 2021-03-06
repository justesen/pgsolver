open Stratimpralgs;;
open Paritygame;;
open Tcsset;;
open Tcsbasedata;;
open Univsolve;;
open Tcsarray;;
open Transformations;;




let multiple_edge_transformation game =
	let n = pg_size game in
	let mult = Array.make n [||] in
	let game' = pg_copy game in
	for i = 0 to n - 1 do
		let (pr, _, tr, _) = game.(i) in
		if (pr = 0) && (Array.length tr = 1)
		then game'.(i) <- (-1, -1, [||], None)
	done;
	for i = 0 to n - 1 do
		let (pr, _, tr, _) = game'.(i) in
		if (pr >= 0) then (
			let s = ref TreeMap.empty_def in
			Array.iter (fun r ->
				let r = ref r in
				while (pg_get_pr game' !r < 0) do
					r := (pg_get_tr game !r).(0)
				done;
				try
					let q = TreeMap.find !r !s in
					s := TreeMap.add !r (q + 1) !s
				with
					Not_found -> s := TreeMap.add !r 1 !s
			) tr;
			let (tr, mu) = (ref [], ref []) in
			TreeMap.iter (fun r m ->
				tr := r::!tr;
				mu := m::!mu
			) !s;
			pg_set_tr game' i (Array.of_list !tr);
			mult.(i) <- Array.of_list !mu
		)
	done;
	(game', mult)

let multiple_edge_backtransformation game solution strategy =
	Array.iteri (fun i (pr, pl, tr, _) ->
		if (pr >= 0) && (solution.(i) < 0) then (
			solution.(i) <- solution.(tr.(0));
			if solution.(i) = pl
			then strategy.(i) <- tr.(0)
		)
	) game

let multiple_edge_solve game solver =
	let (game', mult) = multiple_edge_transformation game in
	let (game'', new2old, old2new) = compress_nodes game' in
	let n = Array.length game' in
	let n' = Array.length game'' in
	let mult' = Array.make n' [||] in
	for i = 0 to n' - 1 do
		mult'.(i) <- mult.(new2old.(i))
	done;
	let (sol', strat') = solver game'' mult' in
	let sol = Array.init n (fun i ->
		if old2new.(i) = -1 then -1 else sol'.(old2new.(i))
	) in
	let strat = Array.init n (fun i ->
		if old2new.(i) = -1 then -1
		else let j = strat'.(old2new.(i)) in
		     if j = -1 then -1 else new2old.(j)
	) in
	multiple_edge_backtransformation game sol strat;
	(sol, strat)



let improvement_policy_vorobyov2_init_edges game =
	let ed = ref (TreeSet.empty compare) in
	Array.iteri (fun i (pr, pl, tr, _) ->
		if (pr >= 0) && (pl = 0) then Array.iter (fun j ->
			ed := TreeSet.add (i,j) !ed
		) tr		
	) game;
	[(pg_copy game, !ed, TreeSet.empty compare, 0)]

let improvement_policy_vorobyov2 game node_compare
                                 stack
                                 strategy valu =
	let rec iterate = function
		[] -> (strategy, [])
	|	(ga, ed, av, nu)::stack -> (
		if strategy_improvable ga node_compare strategy valu then (
			let strate = ref (TreeSet.empty compare) in
			let ga' = Array.mapi (fun i (pr, pl, tr, de) ->
				if (pr >= 0) && (pl = 0) then strate := TreeSet.add (i,strategy.(i)) !strate;
				let tr' = if (pr >= 0) && (pl = 0) then [|strategy.(i)|] else tr in
				(pr, pl, tr', de)
			) ga in
			let entry = (ga', !strate, ed, max (TreeSet.cardinal !strate) (TreeSet.cardinal ed / 2)) in
			iterate (entry::(ga, ed, av, nu)::stack)
		)
		else if nu > 0 then (
			let impr_edges = Array.of_list (List.filter (fun (i,j) ->
				node_valuation_ordering ga node_compare valu.(j) valu.(strategy.(i)) > 0
			) (TreeSet.elements av)) in
			if Array.length impr_edges = 0 then iterate stack
			else (
				let (i,j) = impr_edges.(Random.int (Array.length impr_edges)) in
				let strategy' = Array.copy strategy in
				strategy'.(i) <- j;
				pg_set_tr ga i (Array.of_list (j::(Array.to_list (pg_get_tr ga i))));
				(strategy', (ga, TreeSet.add (i,j) ed, TreeSet.remove (i,j) av, nu - 1)::stack)
			)
		)
		else iterate stack
	)
	in	
	iterate stack                                 

                                

let improvement_policy_vorobyovordered game node_total_ordering
                                (edgearr, counter, stack, edgeord)
                                old_strategy valu =

	let strategy = Array.copy old_strategy in

	while !counter > 0 do
		let j = ref 0 in
		decr counter;
		let working = ref true in
		while !working do
			let (x,y) = edgeord.(!j) in
			let (i, tr) = edgearr.(x) in
			let k = ArrayUtils.index_of tr y in
			if k < !i then (
				incr j
			) else (
				working := false;
				let tmp = tr.(!i) in
				tr.(!i) <- tr.(k);
				tr.(k) <- tmp;
				incr i;
				stack := x::!stack
			)
		done;
	done;

	let unaltered = ref true in

	while (!stack != []) && !unaltered do
		let e = List.hd !stack in
		stack := List.tl !stack;
		incr counter;
		let (i, tr) = edgearr.(e) in
		decr i;
		let se = tr.(0) in
		let ee = tr.(!i) in
		if (node_valuation_ordering game node_total_ordering valu.(se) valu.(ee) < 0) then (
			strategy.(e) <- ee;
			tr.(0) <- ee;
			tr.(!i) <- se;
			unaltered := false
		)
	done;

	(strategy, (edgearr, counter, stack, edgeord));;


let improvement_policy_vorobyovordered_init_edges game init_strat =
	Random.self_init ();
	let counter = ref 0 in
	let ord = ref [] in
	let edgearr = Array.mapi (fun i (pr, pl, tr, _) ->
		if pl = 0 && pr >= 0 then (
			Array.iter (fun j -> ord := (i,j)::!ord) tr;
            let j = ArrayUtils.index_of tr init_strat.(i) in
            let tr' = Array.copy tr in
            tr'.(0) <- tr.(j);
            tr'.(j) <- tr.(0);
            counter := !counter + Array.length tr' - 1;
            (ref 1, tr')
        )
        else (ref 0, [||])
	) game in
	(edgearr, counter, ref [], (ArrayUtils.shuffle (Array.of_list !ord)))
	

let improvement_policy_randomizedbland game node_total_ordering ordering old_strategy valu =
	let strategy = Array.copy old_strategy in
	let cmp i j =
		node_valuation_ordering game node_total_ordering valu.(i) valu.(j)
	in
	let idx = ArrayUtils.find (fun (x,y) -> cmp y strategy.(x) > 0) ordering in
	let (x,y) = ordering.(idx) in
	strategy.(x) <- y;
	(strategy, ordering)

	
let improvement_policy_randomizedbland_init_edges game init_strat =
	Random.self_init ();
	let ord = ref [] in
	Array.iteri (fun i (pr, pl, tr, _) ->
		if pl = 0 && pr >= 0
		then Array.iter (fun j -> ord := (i,j)::!ord) tr;
	) game;
	(ArrayUtils.shuffle (Array.of_list !ord))
	
let improvement_policy_vorobyov game node_total_ordering
                                (edgearr, counter, stack)
                                old_strategy valu =

	let strategy = Array.copy old_strategy in

	while !counter > 0 do
		let j = ref (Random.int !counter) in
		decr counter;
		let k = ref 0 in
		let working = ref true in
		while !working do
			let (i, tr) = edgearr.(!k) in
			let c = Array.length tr - !i in
			if c > !j then (
				working := false;
				let h = !i + !j in
				let tmp = tr.(!i) in
				tr.(!i) <- tr.(h);
				tr.(h) <- tmp;
				incr i;
				stack := !k::!stack;
			)
			else (
				j := !j - c;
				incr k
			)
		done;
	done;

	let unaltered = ref true in

	while (!stack != []) && !unaltered do
		let e = List.hd !stack in
		stack := List.tl !stack;
		incr counter;
		let (i, tr) = edgearr.(e) in
		decr i;
		let se = tr.(0) in
		let ee = tr.(!i) in
		if (node_valuation_ordering game node_total_ordering valu.(se) valu.(ee) < 0) then (
			strategy.(e) <- ee;
			tr.(0) <- ee;
			tr.(!i) <- se;
			unaltered := false
		)
	done;

	(strategy, (edgearr, counter, stack));;


let improvement_policy_vorobyov_init_edges game init_strat =
	Random.self_init ();
	let counter = ref 0 in
	let edgearr = Array.mapi (fun i (pr, pl, tr, _) ->
		if pl = 0 && pr >= 0 then (
            let j = ArrayUtils.index_of tr init_strat.(i) in
            let tr' = Array.copy tr in
            tr'.(0) <- tr.(j);
            tr'.(j) <- tr.(0);
            counter := !counter + Array.length tr' - 1;
            (ref 1, tr')
        )
        else (ref 0, [||])
	) game in
	(edgearr, counter, ref [])


let vorobyov_map_multiplicity arr =
	let c = ref 0 in
	Array.iter (fun (_, m) -> c := !c + m) arr;
	let a = Array.make !c (-1) in
	let j = ref 0 in
	Array.iter (fun (e, m) ->
		for i = 1 to m do
			a.(!j) <- e;
			incr j
		done
	) arr;
	a

let policy_vorobyov_multiple_edges_init_edges game init_strat multiplicities =
	let comb a b = Array.mapi (fun i x -> (x, b.(i))) a in
	Random.self_init ();
	let counter = ref 0 in
	let edgearr = Array.mapi (fun i (pr, pl, tr, _) ->
		if pl = 0 && pr >= 0 then (
			let tr = vorobyov_map_multiplicity (comb tr multiplicities.(i)) in
            let j = ArrayUtils.index_of tr init_strat.(i) in
            let tr' = Array.copy tr in
            tr'.(0) <- tr.(j);
            tr'.(j) <- tr.(0);
            counter := !counter + Array.length tr' - 1;
            (ref 1, tr')
        )
        else (ref 0, [||])
	) game in
	(edgearr, counter, ref [])




let improvement_policy_single_randomly game node_total_ordering old_strategy valu =
	let strategy = Array.copy old_strategy in
	let cmp i j =
		node_valuation_ordering game node_total_ordering valu.(i) valu.(j)
	in
	let edges = ref [] in
	Array.iteri (fun i j ->
		if j != -1 then
			Array.iter (fun k ->
				if cmp j k < 0 then edges := (i,k)::!edges
			) (pg_get_tr game i)
	) strategy;
	let edges_arr = Array.of_list !edges in
	let len = Array.length edges_arr in
	if len > 0 then (
		let (i, j) = edges_arr.(Random.int len) in
		strategy.(i) <- j
	);
	strategy

let improvement_policy_single_node_edge_randomly game node_total_ordering old_strategy valu =
	let strategy = Array.copy old_strategy in
	let cmp i j =
		node_valuation_ordering game node_total_ordering valu.(i) valu.(j)
	in
	let node_edges = ref [] in
	Array.iteri (fun i j ->	if j != -1 then (
        let edges = ref [] in
        Array.iter (fun k ->
            if cmp j k <= 0 then edges := k::!edges
        ) (pg_get_tr game i);
        node_edges := (i, !edges)::!node_edges
	)) strategy;
	let node_edges_arr = Array.of_list !node_edges in
	let len = Array.length node_edges_arr in
	if len > 0 then (
		let (i, edges) = node_edges_arr.(Random.int len) in
        let edges_arr = Array.of_list edges in
        let len = Array.length edges_arr in
        if len > 0 then (
            let j = edges_arr.(Random.int len) in
            strategy.(i) <- j
        )
	);
	strategy

let improvement_policy_all_randomly game node_total_ordering old_strategy valu =
	let n = pg_size game in
	let strategy = Array.copy old_strategy in
	let cmp i j =
		node_valuation_ordering game node_total_ordering valu.(i) valu.(j)
	in
	let improvable = ref true in
	let same = ref true in
	while (!improvable && !same) do
		improvable := false;
        for i = 0 to n - 1 do
            if strategy.(i) > -1 then (
                let a = ArrayUtils.filter (fun j -> cmp strategy.(i) j <= 0) (pg_get_tr game i) in
                improvable := !improvable || (Array.length a > 1);
                strategy.(i) <- a.(Random.int (Array.length a));
                same := !same && (strategy.(i) = old_strategy.(i))
            )
        done
    done;
	strategy


let strategy_improvement_single_randomly_policy game =
	Random.self_init ();
	strategy_improvement game initial_strategy_by_last_edge node_total_ordering_by_position (improvement_policy_no_user_data improvement_policy_single_randomly) () false "STRIMPR_SIRAND";;

let strategy_improvement_single_node_edge_randomly_policy game =
	Random.self_init ();
	strategy_improvement_by_policy game (improvement_policy_no_user_data improvement_policy_single_node_edge_randomly) () false "STRIMPR_SINOED";;

let strategy_improvement_all_randomly_policy game =
	Random.self_init ();
	strategy_improvement_by_policy game (improvement_policy_no_user_data improvement_policy_all_randomly) () false "STRIMPR_RANDOM";;



let strategy_improvement_vorobyov_policy game =
	let init = initial_strategy_by_best_reward game in
	strategy_improvement game (fun _ -> init) node_total_ordering_by_position improvement_policy_vorobyov (improvement_policy_vorobyov_init_edges game init) false "VOROBYOV";;

let strategy_improvement_vorobyovordered_policy game =
	let init = initial_strategy_by_best_reward game in
	strategy_improvement game (fun _ -> init) node_total_ordering_by_position improvement_policy_vorobyovordered (improvement_policy_vorobyovordered_init_edges game init) false "VOROBYOVORDERED";;

let strategy_improvement_randomizedbland_policy game =
	let init = initial_strategy_by_best_reward game in
	strategy_improvement game (fun _ -> init) node_total_ordering_by_position improvement_policy_randomizedbland (improvement_policy_randomizedbland_init_edges game init) false "RANDOMIZEDBLAND";;

let strategy_improvement_vorobyov2_policy game =
	strategy_improvement'' game initial_strategy_by_best_reward node_total_ordering_by_position improvement_policy_vorobyov2 (fun g -> improvement_policy_vorobyov2_init_edges g) false "VOROBYOV2";;



(*
register_sub_solver
	(fun g -> universal_solve (universal_solve_init_options_verbose !universal_solve_global_options) strategy_improvement_single_node_edge_randomly_policy g)
	"strimprsinera" "sisn" "use strategy improvement with single node/edge randomized i.p.";;
*)
(*
let improvement_vorobyov_policy_multiple_edges game =
	multiple_edge_solve game (fun game mult ->
		let init = initial_strategy_by_best_reward game in
		strategy_improvement game (fun _ -> init) node_total_ordering_by_position improvement_policy_vorobyov (policy_vorobyov_multiple_edges_init_edges game init mult) false "VOROBYOVMULT"
	);;

register_sub_solver
	(fun g -> universal_solve (universal_solve_init_options_verbose !universal_solve_global_options) improvement_vorobyov_policy_multiple_edges g)
	"randomfacetmult" "rfm" "random facet policy iteration with multiple edge transf.";;
	*)



register_sub_solver
	(fun g -> universal_solve (universal_solve_init_options_verbose !universal_solve_global_options) strategy_improvement_vorobyov_policy g)
	"randomfacet" "rf" "random facet policy iteration";;

register_sub_solver
	(fun g -> universal_solve (universal_solve_init_options_verbose !universal_solve_global_options) strategy_improvement_vorobyovordered_policy g)
	"randomfacetord" "rfo" "random facet ordered policy iteration";;

register_sub_solver
	(fun g -> universal_solve (universal_solve_init_options_verbose !universal_solve_global_options) strategy_improvement_vorobyov2_policy g)
	"dualrandomfacet" "drf" "dual random facet policy iteration";;

register_sub_solver
	(fun g -> universal_solve (universal_solve_init_options_verbose !universal_solve_global_options) strategy_improvement_randomizedbland_policy g)
	"randombland" "rb" "randomized blands rule policy iteration";;

register_sub_solver
	(fun g -> universal_solve (universal_solve_init_options_verbose !universal_solve_global_options) strategy_improvement_all_randomly_policy g)
	"switchhalf" "sh" "switch every edge with probabiliby 0.5 policy iteration";;

register_sub_solver
	(fun g -> universal_solve (universal_solve_init_options_verbose !universal_solve_global_options) strategy_improvement_single_randomly_policy g)
	"randomedge" "re" "random edge strategy iteration";;
