open Arytmetyka

let a = wartosc_od_do 2. 9.;;                (* [2., 9.]                      *)
let b = wartosc_od_do (-2.5) 10.;;           (* [-2.5, 10.]                   *)
let c = podzielic a b;;                      (* [-inf, -0.8] U [0.2, inf]     *)
let d = podzielic c b;;                      (* [-inf, -0.08] U [0.02, inf]   *)
let e = plus d (wartosc_dokladna 2.);;       (* [-inf, 1.92] U [2.02, inf]    *)
let f = razy d b;;                           (* [-inf, inf]                   *)
let g = minus d (wartosc_dokladna 3.);;      (* [-inf, -3.08] U [-2.98, inf]  *)
let h = wartosc_dokladna 0.;;                (* [0., 0.]                      *)
let i = wartosc_dokladna (-0.);;             (* [0., 0.]                      *)
let j = podzielic f i;;                      (* empty set                     *)
let k = podzielic b h;;                      (* empty set                     *)
let l = razy g (wartosc_dokladna (1.1));;    (* [-inf, -3.63] U [-3.168, inf] *)
let m = sr_wartosc ( podzielic ( wartosc_dokladnosc (-1.200000) (4.800000) ) ( wartosc_dokladnosc (-1.800000) (0.000000) ) ) ;;
let n = sr_wartosc ( razy ( podzielic ( wartosc_od_do (-7.200000) (0.000000) ) ( razy ( wartosc_dokladnosc (-1.000000) (4.000000) ) ( razy ( wartosc_dokladna (8.400000) ) ( wartosc_dokladnosc (-9.800000) (2.600000) ) ) ) ) ( wartosc_dokladnosc (7.400000) (1.200000) ) ) ;;



let eps = 0.000001;;
let ( =. ) (x : float) (y : float) = (x +. eps > y) && (x -. eps < y);;

assert(min_wartosc a =. 2.0);;
assert(max_wartosc a =. 9.0);;
assert(in_wartosc a 2.);;
assert(in_wartosc a 3.);;
assert(not (in_wartosc a (-3.)));;
assert(not (in_wartosc a 4782782222.));;

assert(sr_wartosc b =. 3.75);;
assert(min_wartosc b =. -2.5);;
assert(max_wartosc b =. 10.);;
assert(in_wartosc b (5.));;

assert(not (in_wartosc c 0.));;
assert(in_wartosc c 100.);;

assert(compare (sr_wartosc d) nan = 0);;
assert(in_wartosc d (-3. /. 11.));;
assert(max_wartosc d = infinity);;
assert(min_wartosc d = neg_infinity);;

assert(in_wartosc e 0.);;
assert(in_wartosc e 1.74444);;
assert(in_wartosc e 3.13212);;
assert(not (in_wartosc e 1.92001));;
assert(not (in_wartosc e 2.0199));;
assert(in_wartosc e 2.02);;

assert(in_wartosc f 123.456);;
assert(in_wartosc f (-123.456));;
assert(in_wartosc f (-737823782.287272));;
assert(compare (sr_wartosc f) nan = 0);;
assert(in_wartosc f 0.0);;
assert(in_wartosc f (-0.0));;
assert(min_wartosc f = neg_infinity);;
assert(max_wartosc f = infinity);;

assert((min_wartosc h) =. 0.);;
assert((max_wartosc h) =. 0.);;
assert((sr_wartosc h) =. 0.);;
assert((min_wartosc h) =. (min_wartosc i));;
assert((max_wartosc h) =. (max_wartosc i));;
assert((sr_wartosc h) =. (sr_wartosc i));;

assert(compare (min_wartosc j) (min_wartosc k) = 0);;
assert(compare (max_wartosc j) (max_wartosc k) = 0);;
assert(compare (min_wartosc j) nan = 0);;
assert(compare (max_wartosc j) nan = 0);;
assert(compare (sr_wartosc j) (sr_wartosc k) = 0);;
assert(compare (sr_wartosc j) nan = 0);;

assert(compare (sr_wartosc l) nan = 0);;
assert(in_wartosc l (-3.63));;
assert(in_wartosc l (-3.168));;
assert(in_wartosc l 0.0);;

assert (m =. 0.666666666666666741);;
assert (n =. -0.350250836620949302);;

print_endline "OK"