(* Zadanie: Sortowanie Topologiczne *)
(* Autor: Michał Niedziółka *)
(* Code review: Michał Szczęśniak *)

open PMap
exception Cykliczne

let topol l =
    begin
    (* graf - mapa list sąsiedztwa | kolor - mapa kolorów wierzchołkow 0 -> nieodwiedzony, 1 -> na jednej spójnej z aktualnym, 2 -> odwiedzony *)
    let graf = ref (List.fold_left (fun m (k, s) -> add k s m) empty l)
    and kolor = ref (List.fold_left (fun m (k, s) -> add k 0 m) empty l) 
    and wynik = ref [] in
    let rec dfs v =
        let akt_kolor = try find v !kolor with _ -> begin kolor := add v 0 !kolor; 0 end
        in 
        (* graf jest cykliczny jeśli w jednym przejściu dfsa trafie na wierzcholek juz odwiedzony w tym przejsciu*)
        if akt_kolor = 1 then raise Cykliczne 
        else
            if akt_kolor <> 2 then 
                begin
                    (* zmien kolor na 1 przed pojsciem dalej *)
                    kolor := add v 1 (remove v !kolor);
                    List.iter (fun el -> (dfs el)) (try find v !graf with _ -> []);
                    (* zmien kolor na 2 przed wyjsciem *)
                    kolor := add v 2 (remove v !kolor);
                    wynik := v::(!wynik)
                end
    in iter (fun k _ -> if (find k !kolor) <> 2 then (dfs k) ) !graf;
    !wynik
    end
;;
