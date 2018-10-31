(* Arytmetyka przybliżonych wartości *)
(* Autor: Michał Niedziółka *)
(* Code review: Marcin Abramowicz *)

(* --------TYP-------- *)

(* lewa prawa -> końce przedziału *)
(* czyodwrocony -> czy [a, b] czy [-inf, a] U [b, inf] *)
(* czypusty -> czy nan *)

type wartosc = {
    lewa : float; 
    prawa : float; 
    czyodwrocony : bool; 
    czypusty : bool;
}

(* --------POMOCNICZE------- *)

let pusty = {lewa = neg_infinity; prawa = infinity; czyodwrocony = true; czypusty = true}

let nieskonczony = {lewa = neg_infinity; prawa = infinity; czyodwrocony = false; czypusty = false}

let min4 a b c d =
    min (min a b) (min c d)
;;

let max4 a b c d =
    max (max a  b) (max c d)
;;

(* Mnożenie rozwiązujące 0 * infinity *)
let mnozenie x y = 
    if (x=neg_infinity || x=infinity) && y=0. then 0.
    else if (y=neg_infinity || y=infinity) && x=0. then 0.

    else x *. y
;;

(* Funkcja sprawdzająca czy zbiór jest zbiorem (-inf, inf) *)
let sprawdz x = 
    if x.czyodwrocony = true
    then if x.lewa >= x.prawa then nieskonczony else x
    else x
;;

(* Mnożenie przedziału odwróconego i nieodwróconego *)
let pomnoz_odwrocony_normalny a b = 
    if b.lewa <= 0. && b.prawa <= 0.
    then
        sprawdz {
            lewa = mnozenie a.prawa b.prawa;
            prawa = mnozenie a.lewa b.prawa;
            czyodwrocony = true; czypusty = false
        }     
    else if b.lewa <= 0. && b.prawa >= 0.
        then
            nieskonczony 
        else
            sprawdz {
                lewa = mnozenie a.lewa b.lewa;
                prawa = mnozenie a.prawa b.lewa;
                czyodwrocony = true; czypusty = false
            }
;;

(* Równość floatów *)
let eps = 0.000000000000000001;;

let okolo w x = 
    (w +. eps > x) && (w -. eps < x)
;;

let negacja temp = sprawdz {
    lewa = temp.prawa *. -1.;
    prawa = temp.lewa *. -1.;
    czyodwrocony = temp.czyodwrocony;
    czypusty = temp.czypusty
};;

(* --------KONSTRUKTORY-------- *)
let wartosc_od_do x y = {
    lewa = x; 
    prawa = y; 
    czyodwrocony = false; 
    czypusty = false
};;

let wartosc_dokladnosc x p = 
    wartosc_od_do (x -. abs_float (mnozenie x p /. 100.)) (x +. abs_float (mnozenie x p /. 100.))
;;

let wartosc_dokladna x = {
    lewa = x; 
    prawa = x; 
    czyodwrocony = false; 
    czypusty = false
};;


(* --------SELEKTORY-------- *)
let in_wartosc w x =
    if w.czypusty
    then
	    false
    else if w.czyodwrocony
    then if w.lewa >= w.prawa
        then
            true
        else 
            (x <= (w.lewa)) || ((w.prawa) <= x) || okolo w.lewa x || okolo w.prawa x
    else 
        (((w.lewa) <= x) && (x <= (w.prawa)) || okolo w.lewa x || okolo w.prawa x)
;;

let min_wartosc w =
    if w.czypusty
    then nan
    else if w.czyodwrocony
    then neg_infinity
    else w.lewa
;;

let max_wartosc w =
    if w.czypusty
    then nan
    else if w.czyodwrocony
    then infinity
    else w.prawa
;;

let sr_wartosc w =
    if w.czypusty
    then nan
    else if w.czyodwrocony
    then nan
    else (min_wartosc w +. max_wartosc w) /. 2.
;;

(* --------MODYFIKATORY-------- *)
let plus a b =
    if a.czypusty || b.czypusty
    then pusty
    else if a.czyodwrocony
    then
        if b.czyodwrocony
        then
            nieskonczony
        else
            sprawdz {lewa = a.lewa +. b.prawa; prawa = a.prawa +. b.lewa; czyodwrocony = true; czypusty = false}
    else
	if b.czyodwrocony
	then
	    sprawdz {lewa = b.lewa +. a.prawa; prawa = b.prawa +. a.lewa; czyodwrocony = true; czypusty = false}
	else
	    sprawdz {lewa = a.lewa +. b.lewa; prawa = a.prawa +. b.prawa; czyodwrocony = false; czypusty = false}
;;

let minus a b =
    sprawdz (plus a (negacja b))
;;

let razy a b =
    if a.czypusty || b.czypusty
    then pusty
    else if (okolo a.lewa 0. && okolo a.prawa 0.) || (okolo b.lewa 0. && okolo b.prawa 0.)
    then
        wartosc_dokladna 0.
    else if a.czyodwrocony
        then if b.czyodwrocony
                then if (a.lewa > 0. || a.prawa < 0.) || (b.lewa > 0. || b.prawa < 0.) (* i czy lub *)
                then
                    nieskonczony
                else
                    sprawdz {
                        lewa = max (mnozenie a.lewa b.prawa) (mnozenie b.lewa a.prawa);
                        prawa = min (mnozenie a.lewa b.lewa) (mnozenie a.prawa b.prawa);
                        czyodwrocony = true;
                        czypusty = false
                    }
            else pomnoz_odwrocony_normalny a b
        else if b.czyodwrocony
        then pomnoz_odwrocony_normalny b a
        else sprawdz {
            lewa = min4 (mnozenie a.lewa b.lewa) (mnozenie a.lewa b.prawa) (mnozenie a.prawa b.lewa) (mnozenie a.prawa b.prawa);
            prawa = max4 (mnozenie a.lewa b.lewa) (mnozenie a.lewa b.prawa) (mnozenie a.prawa b.lewa) (mnozenie a.prawa b.prawa);
            czyodwrocony = false;
            czypusty = false
        }

;;

let odwrotnosc a = 
    if in_wartosc a 0.
    then
        if a.czyodwrocony
        then 
            if a.lewa >= a.prawa
            then nieskonczony
            else if okolo a.lewa 0.
            then
                sprawdz {lewa = neg_infinity; prawa = 1. /. a.prawa; czyodwrocony = false; czypusty = false}
            else if okolo a.prawa 0.
            then
                sprawdz {lewa = 1. /. a.lewa; prawa = infinity; czyodwrocony = false; czypusty = false}
            else
                sprawdz {lewa = 1. /. a.prawa; prawa = 1. /. a.lewa; czyodwrocony = true; czypusty = false} 
        else
            if a.lewa = neg_infinity && a.prawa = infinity
            then nieskonczony
            else if okolo a.lewa 0.
            then 
                sprawdz {lewa = 1. /. a.prawa; prawa = infinity; czyodwrocony = false; czypusty = false}
            else if okolo a.prawa 0.
            then 
                sprawdz {lewa = neg_infinity; prawa = 1. /. a.lewa; czyodwrocony = false; czypusty = false} 
            else
                sprawdz {lewa = 1. /. a.lewa; prawa = 1. /. a.prawa; czyodwrocony = true; czypusty = false}
    else
        sprawdz {
            lewa = (1. /. a.prawa); 
            prawa = (1. /. a.lewa);
            czyodwrocony = false;
            czypusty = false
        }
;;

let podzielic a b = 
    if a.czypusty || b.czypusty || (okolo b.lewa 0. && okolo b.prawa 0.)
    then
        pusty
    else
        sprawdz(razy a (odwrotnosc b))
;;


(* TESTS *)

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
