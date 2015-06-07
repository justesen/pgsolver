type paritygame = (int * int * int array * string option) array
type solution = int array
type strategy = int array

val attr             : paritygame -> strategy -> int -> int list -> int list
val is_null          : int * int * int array * string option -> bool 
val win_nodes        : int -> solution -> int list
val add_to_win_reg   : int -> int -> int list -> solution -> solution
val merge_strategies : int -> strategy list -> strategy
val sanify_strat     : paritygame -> strategy -> solution -> strategy
