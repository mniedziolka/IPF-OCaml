(* --------TYP-------- *)

type wartosc = {
    lewa : float; 
    prawa : float; 
    czyodwrocony : bool; 
    czypusty : bool;
}

(* --------POMOCNICZE------- *)

let min4 a b c d =
    min (min a b) (min c d)
;;

let max4 a b c d =
    max (max a  b) (max c d)
;;

let mnozenie x y = 
    if (x=neg_infinity || x=infinity) && y=0. then 0.
    else if (y=neg_infinity || y=infinity) && x=0. then 0.

    else x *. y
;;

let pomnoz_odwrocony_normalny a b = 
    if b.lewa <= 0. && b.prawa <= 0.
    then
        {
            lewa = mnozenie a.prawa b.prawa;
            prawa = mnozenie a.lewa b.prawa;
            czyodwrocony = true; czypusty = false
        }     
    else if b.lewa <= 0. && b.prawa >= 0.
        then
            {lewa = 1.; prawa = -1.; czyodwrocony = true; czypusty = false}   
        else
            {
                lewa = mnozenie a.lewa b.lewa;
                prawa = mnozenie a.prawa b.lewa;
                czyodwrocony = true; czypusty = false
            }
;;

let eps = 0.0000001;;

let okolo w x = 
    (w +. eps > x) && (w -. eps < x)
;;

let negacja temp = {
    lewa = temp.prawa *. -1.;
    prawa = temp.lewa *. -1.;
    czyodwrocony = temp.czyodwrocony;
    czypusty = temp.czypusty
};;

let pusty = {lewa = neg_infinity; prawa = infinity; czyodwrocony = true; czypusty = true}



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
            (x <= (w.lewa)) || ((w.prawa) <= x)
    else
        (((w.lewa) < x) && (x < (w.prawa)) || okolo w.lewa x || okolo w.prawa x)
;;

let min_wartosc w =
    if w.czypusty
    then
	nan
    else if w.czyodwrocony
    then
        neg_infinity
    else
        w.lewa
;;

let max_wartosc w =
    if w.czypusty
    then
	nan
    else if w.czyodwrocony
    then
        infinity
    else
        w.prawa
;;

let sr_wartosc w =
    if w.czypusty
    then
	nan
    else if w.czyodwrocony
    then
        nan
    else
        (min_wartosc w +. max_wartosc w) /. 2.
;;

(* --------MODYFIKATORY-------- *)
let plus a b =
    if a.czypusty || b.czypusty
    then pusty
    else if a.czyodwrocony
    then
        if b.czyodwrocony
        then
            {lewa = 1.; prawa = -1.; czyodwrocony = true; czypusty = false}
        else
            {lewa = a.lewa +. b.prawa; prawa = a.prawa +. b.lewa; czyodwrocony = true; czypusty = false}
    else
	if b.czyodwrocony
	then
	    {lewa = b.lewa +. a.prawa; prawa = b.prawa +. a.lewa; czyodwrocony = true; czypusty = false}
	else
	    {lewa = a.lewa +. b.lewa; prawa = a.prawa +. b.prawa; czyodwrocony = false; czypusty = false}
;;

let minus a b =
    plus a (negacja b)
;;

let razy a b =
    if a.czypusty || b.czypusty
    then pusty
    else if (okolo a.lewa 0. && okolo a.prawa 0.) || (okolo b.lewa 0. && okolo b.prawa 0.)
    then
        wartosc_dokladna 0.
    else if a.czyodwrocony
        then if b.czyodwrocony
                then if (a.lewa > 0. || a.prawa < 0.) && (b.lewa > 0. || b.prawa < 0.) (* i czy lub *)
                then
                    {lewa = 2.; prawa = -2.; czyodwrocony = true; czypusty = false}
                else
                    {
                        lewa = max (mnozenie a.lewa b.prawa) (mnozenie b.lewa a.prawa);
                        prawa = min (mnozenie a.lewa b.lewa) (mnozenie a.prawa b.prawa);
                        czyodwrocony = true;
                        czypusty = false
                    }
            else pomnoz_odwrocony_normalny a b
        else if b.czyodwrocony
        then pomnoz_odwrocony_normalny b a
        else {
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
            then a
            else if okolo a.lewa 0.
            then
                {lewa = neg_infinity; prawa = 1. /. a.prawa; czyodwrocony = false; czypusty = false}
            else if okolo a.prawa 0.
            then
                {lewa = 1. /. a.lewa; prawa = infinity; czyodwrocony = false; czypusty = false}
            else
                {lewa = 1. /. a.prawa; prawa = 1. /. a.lewa; czyodwrocony = true; czypusty = false} 
        else
            if a.lewa = neg_infinity && a.prawa = infinity
            then a
            else if okolo a.lewa 0.
            then 
                {lewa = 1. /. a.prawa; prawa = infinity; czyodwrocony = false; czypusty = false}
            else if okolo a.prawa 0.
            then 
                {lewa = neg_infinity; prawa = 1. /. a.lewa; czyodwrocony = false; czypusty = false} 
            else
                {lewa = 1. /. a.lewa; prawa = 1. /. a.prawa; czyodwrocony = true; czypusty = false}
    else
        {
            lewa = (1. /. a.prawa); 
            prawa = (1. /. a.lewa);
            czyodwrocony = false;
            czypusty = false
        }
;;

let podzielic a b = 
    if a.czypusty || b.czypusty || (okolo a.lewa 0. && okolo a.prawa 0.) || (okolo b.lewa 0. && okolo b.prawa 0.)
    then
        pusty
    else
        razy a (odwrotnosc b)
;;
