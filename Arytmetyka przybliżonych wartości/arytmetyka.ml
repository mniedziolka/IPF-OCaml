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

let razy a b =
    if a.czypusty || b.czypusty
    then
        {lewa = neg_infinity; prawa = infinity; czyodwrocony = true; czypusty = true}
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
            {lewa = min4 (a.lewa *. b.lewa) (a.lewa *. b.prawa) (a.prawa *. b.lewa) (a.prawa *. b.prawa); 
            prawa = max4 (a.lewa *. b.lewa) (a.lewa *. b.prawa) (a.prawa *. b.lewa) (a.prawa *. b.prawa); 
            czyodwrocony = false; czypusty = false}
;;

let odwotnosc a = 
    
;;
