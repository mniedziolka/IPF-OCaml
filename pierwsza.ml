let czypierwsza n = 
        let rec temp a =
                if a * a > n then true
                else if n mod a = 0 then false
                else temp (a + 1)
        in (n > 1) && (temp 2);;

czypierwsza 2;;
czypierwsza 4;;
czypierwsza 13;;
czypierwsza 1;;
