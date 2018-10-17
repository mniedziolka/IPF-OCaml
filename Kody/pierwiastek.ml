let sqrt n = 
        let rec temp k i =
                if i > n then k - 1 else temp (k + 1) (i + 2 * k + 1)
        in temp 0 0
;;
