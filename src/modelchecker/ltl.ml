type variable = string

type ltlexpr =
    | TT
    | FF
    | Var of variable
    | Neg of ltlexpr
    | Con of ltlexpr * ltlexpr
    | Dis of ltlexpr * ltlexpr
    | Next of ltlexpr
    | Until of ltlexpr * ltlexpr
    | Eventually of ltlexpr
    | Always of ltlexpr


(* to_mucalc : ltlexpr -> muexpr *)
let rec to_mucalc = function
    | TT             -> Mucalc.TT
    | FF             -> Mucalc.FF
    | Var x          -> Mucalc.Var x
    | Neg e          -> Mucalc.Neg (to_mucalc e)
    | Con (e1, e2)   -> Mucalc.Con (to_mucalc e1, to_mucalc e2)
    | Dis (e1, e2)   -> Mucalc.Dis (to_mucalc e1, to_mucalc e2)
    | Next e         -> Mucalc.ForAll ("", to_mucalc e)
    | Until (e1, e2) -> Mucalc.LFP ("x", Mucalc.Dis (to_mucalc e2, Mucalc.Con (to_mucalc e1, Mucalc.ForAll ("", Mucalc.Var "x"))))
    | Eventually e   -> Mucalc.LFP ("x", Mucalc.Dis (to_mucalc e, Mucalc.ForAll ("", Mucalc.Var "x"))) (* to_mucalc (Until (TT, e)) *)
    | Always e       -> Mucalc.GFP ("x", Mucalc.Con (to_mucalc e, Mucalc.ForAll ("", Mucalc.Var "x"))) (* to_mucalc (Neg (Eventually (Neg e))) *)