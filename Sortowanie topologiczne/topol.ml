(* Zadanie: Sortowanie Topologiczne *)
(* Autor: Michał Niedziółka *)
(* Code review: Michał Szczęśniak *)

exception Cykliczne

let topol l =
    let graf = ref (List.fold_left (fun m (k, s) -> PMap.add k s m) PMap.empty l)
    and kolor = ref (List.fold_left (fun m (k, s) -> PMap.add k 0 m) PMap.empty l) in
    let rec dfs v =
        let akt_kolor = PMap.find v !kolor in 
        if akt_kolor = 1 then raise Cykliczne
        else    
        begin
                kolor := PMap.add v 1 (PMap.remove v !kolor);
                let wynik = (List.fold_left (fun a el -> (dfs el)@a) [] (PMap.find v !graf)) in
                kolor := PMap.add v 2 (PMap.remove v !kolor);
                wynik
        end
    in PMap.foldi (fun k _ li -> if (PMap.find k !kolor) = 2 then li else (dfs k)@li ) !graf []
;;

let g = [(1, [2;3]);(3, [6;7]);(6, [4;7]);(7, [5]);(2, [5]);(8, []);(4, []);(5, [])];;
let x = List.iter print_int (topol g);;
