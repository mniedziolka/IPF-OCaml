let czypodzielna n =
    let rec tmp x ak =
        if x = 0 && ak < 10 then ak
        else if x = 0 then tmp ak 0
        else tmp (x / 10) (ak + (x mod 10))
    in tmp n 0
;;