let  zlozenie t  =
    List.fold_left(fun acc f -> fun x -> f(acc x)) (fun x -> x) t
;;
let zlozenie2 li =
    fun x -> List.fold_left(fun a f -> f a ) x li
;;
