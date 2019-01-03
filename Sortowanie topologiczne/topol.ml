(* Zadanie: Sortowanie Topologiczne *)
(* Autor: Michał Niedziółka *)
(* Code review: Michał Szczęśniak *)

exception Cykliczne

let topol l =
    let graf = ref (List.fold_left (fun m (k, s) -> add k s m) empty l)
    and kolor = ref (List.fold_left (fun m (k, s) -> add k 0 m) empty l) in
    let rec dfs v =
        let akt_kolor = find v !kolor in 
        if akt_kolor = 1 then raise Cykliczne
        else
            if akt_kolor = 2 then []
            else 
                begin
                        kolor := add v 1 (remove v !kolor);
                        let wynik = (List.fold_left (fun a el -> (dfs el)@a) [] (find v !graf)) in
                        kolor := add v 2 (remove v !kolor);
                        v::wynik
                end
    in foldi (fun k _ li -> if (find k !kolor) = 2 then li else (dfs k)@li ) !graf []
;;