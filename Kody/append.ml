let append r li1 li2 =
    fold_right (fun h a -> h::a) li1 li2;;