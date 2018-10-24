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
