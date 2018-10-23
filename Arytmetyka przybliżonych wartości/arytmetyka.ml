type wartosc = {lewa : float; prawa : float; czyodwrocony : bool};; 

let wartosc_dokladnosc x p =
    {lewa = x *. (1. -. p /. 100.); prawa = x *. (1. +. p /. 100.); czyodwrocony = false}
;;

let wartosc_od_do x y = 
    {lewa = x; prawa = y; czyodwrocony = false}
;;

let wartosc_dokladna x = 
    {lewa = x; prawa = x; czyodwrocony = false}
;;


let in_wartosc w x = 
    if w.czyodwrocony 
    then
        (x < (w.lewa)) || ((w.prawa) < x)
    else
        ((w.lewa) < x) && (x < (w.prawa))
;;

let min_wartosc w = 
    if w.czyodwrocony
    then
        neg_infinity
    else
        w.lewa
;;

let max_wartosc w =
    if w.czyodwrocony
    then
        infinity        
    else
        w.prawa
;;

let sr_wartosc w =
    if w.czyodwrocony
    then
        nan
    else
        (min_wartosc w +. max_wartosc w) /. 2.0
;;


let plus a b =
    if a.
    if a.czyodwrocony
    then 
        if b.czyodwrocony
        then
            {lewa = neg_infinity; prawa = infinity; czyodwrocony = false}
        else
            {lewa = a.lewa + b.prawa; prawa = }
;;