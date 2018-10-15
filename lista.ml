let rec buduj n =
        if n = 0 then []
        else n :: buduj (n - 1)
;;


