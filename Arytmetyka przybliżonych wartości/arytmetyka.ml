(* --------KONSTRUKTORY-------- *)

type wartosc = {
    lewa : float; 
    prawa : float; 
    czyodwrocony : bool; 
    czypusty : bool;
}

let wartosc_dokladnosc x p = {
    lewa = x *. (1. -. p /. 100.); 
    prawa = x *. (1. +. p /. 100.); 
    czyodwrocony = false; 
    czypusty = false
};;

let wartosc_od_do x y = {
    lewa = x; 
    prawa = y; 
    czyodwrocony = false; 
    czypusty = false
};;

let wartosc_dokladna x = {
    lewa = x; 
    prawa = x; 
    czyodwrocony = false; 
    czypusty = false
};;


let negacja temp = {
    lewa = temp.prawa *. -1.;
    prawa = temp.lewa *. -1.;
    czyodwrocony = temp.czyodwrocony;
    czypusty = temp.czypusty
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
        ((w.lewa) <= x) && (x <= (w.prawa))
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
    then
	{lewa = neg_infinity; prawa = infinity; czyodwrocony = true; czypusty = true}
    else if a.czyodwrocony
    then
        if b.czyodwrocony
        then
            {lewa = neg_infinity; prawa = infinity; czyodwrocony = true; czypusty = false}
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

let pomnoz_odwrocony_normalny a b = {
    lewa = max (a.lewa *. b.lewa) (a.lewa *. b.prawa);
    prawa = min (a.prawa *. b.lewa) (a.prawa *. b.prawa);
    czyodwrocony = true; czypusty = false
};;

let scal a b = {
    lewa = max a.lewa b.lewa;
    prawa = min a.prawa b.prawa;
    czyodwrocony = true;
    czypusty = false
};;

let min4 a b c d =
    min (min a b) (min c d)
;;

let max4 a b c d =
    max (max a  b) (max c d)
;;

let eps = 0.00001;;

let okolo w x = 
    (w +. eps > x) && (w -. eps < x)
;;

let razy a b =
    if a.czypusty || b.czypusty
    then
        {lewa = neg_infinity; prawa = infinity; czyodwrocony = true; czypusty = true}
    else if ((okolo a.lewa 0.) && (okolo a.prawa 0.)) || ((okolo b.lewa 0.) && (okolo b.prawa 0.))
    then
        wartosc_dokladna 0.
    else if a.czyodwrocony
    then
        if b.czyodwrocony
        then
            scal (pomnoz_odwrocony_normalny a b) (pomnoz_odwrocony_normalny b a)
        else
            pomnoz_odwrocony_normalny a b
    else
        if b.czyodwrocony
        then
            pomnoz_odwrocony_normalny b a
        else
            {
                lewa = min4 (a.lewa *. b.lewa) (a.lewa *. b.prawa) (a.prawa *. b.lewa) (a.prawa *. b.prawa); 
                prawa = max4 (a.lewa *. b.lewa) (a.lewa *. b.prawa) (a.prawa *. b.lewa) (a.prawa *. b.prawa); 
                czyodwrocony = false; czypusty = false
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
                (* jeśli lewa > prawej i odwrócony to -neginf + neginf *)
                {lewa = 12.; prawa = 10.; czyodwrocony = true; czypusty = false} 
        else
            if okolo a.lewa 0.
            then 
                {lewa = 1. /. a.prawa; prawa = infinity; czyodwrocony = false; czypusty = false}
            else if okolo a.prawa 0.
            then 
                {lewa = neg_infinity; prawa = 1. /. a.prawa; czyodwrocony = false; czypusty = false} 
            else
                {lewa = 1. /. a.lewa; prawa = 1. /. a.prawa; czyodwrocony = true; czypusty = false}
    else
        {
            (* czy tu nie powinno być na odwrót? min z maxem *)
            lewa = min (1. /. a.lewa) (1. /. a.prawa); 
            prawa = min (1. /. a.lewa) (1. /. a.prawa);
            czyodwrocony = true;
            czypusty = false
        }
;;

let podzielic a b = 
    if a.czypusty || b.czypusty
    then {lewa = neg_infinity; prawa = infinity; czyodwrocony = true; czypusty = true}
    else
        razy a (odwrotnosc b)
;;
