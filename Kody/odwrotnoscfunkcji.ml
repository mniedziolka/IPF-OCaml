(* Niech f: R -> R będzie funkcją:
-bijekcją
-ciągłą
-rosnącą i taką że dla każdego d > 0 f(x + d) - f(x) >= dla
f(0) = 0 *)

let odwrotnosc f = 
    fun y -> let rec szukaj l p =
        let s = (l + p) /. 2.
        in 
        if (obs_float(f(s) -. y) < eps)then
            s
        else if f(s) > y then
            szukaj l s
        else
        szukaj (s + eps) p
    in 
    if(abs_float y.) < eps then 0.
    else if y > 0. then szukaj 0. y
    else szukaj y 0.