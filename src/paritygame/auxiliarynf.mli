type paritygame = (int * int * int array * string option) array
type solution = int array
type strategy = int array

val find_v          : int array -> int array -> int -> int
val win_reg_from_nf : solution -> int -> solution
val strat_from_nf   : solution -> int -> paritygame ->solution
val to_normal_form  : paritygame -> paritygame