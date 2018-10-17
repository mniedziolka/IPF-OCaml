let czypierwsza n = 
        let rec temp a =
                if a * a > n then true
                else if n mod a = 0 then false
                else temp (a + 1)
        in (n > 1) && (temp 2)
;;
