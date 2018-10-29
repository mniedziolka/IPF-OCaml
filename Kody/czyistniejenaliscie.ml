let exist li p =
    list.fold_left (fun a h -> if a = true then a else p h) false li
;;