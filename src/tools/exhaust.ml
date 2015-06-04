let odd n = n mod 2 = 1

let rec munu = function
    | 1            -> "mu x1."
    | n when odd n -> (munu (n-1))^"mu x"^(string_of_int n)^"."
    | n            -> (munu (n-1))^"nu x"^(string_of_int n)^"."

let rec parens n = function
    | i when i = n -> "(q"^(string_of_int n)^" or "^"<>x"^(string_of_int n)^")"
    | i            -> "(q"^(string_of_int i)^" or "^"<>(x"^(string_of_int i)^" and "^(parens n (i+1))^"))"

let phi n =
    let psi = (munu n)^(parens n 1) in
    psi^" or ~("^psi^")"

let () =
    print_string ((phi (int_of_string Sys.argv.(1)))^"\n")