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
    if !w.czyodwrocony 
    then
        w.lewa < x && x < w.prawa
    else
        x < w.lewa || w.prawa < x
;;

let min_wartosc w = 
    if !w.czyodwrocony
    then
        w.lewa
    else
        neg_infinity
;;

let max_wartosc w =
    if !w.czyodwrocony
    then
        w.prawa
    else 
        infinity
;;

let sr_wartosc w =
    if !w.czyodwrocony
    then
        (min_wartosc w +. max_wartosc w) /. 2
    else
        nan
;;