let appendr r li1 li2 =
    fold_right (fun h a -> h::a) li1 li2;;

let appendl l1 l2 = 
    let reverse xs = List.fold_left (fun y ys -> y::ys) [] xs
    in reverse (List.fold_left(fun y ys -> y::ys) (reverse li1) l2);;