let rec mod11 x = 
    let rec pom r1 r2 x =     
        if x = 0 then r1 - r2 else pom (r1 + x mod 10) (r2 + (x / 10) mod 10) (x / 100)  
    in 
        let r = pom 0 0 x
        in 
            if r > 10 then mod11 r
            else if r < 0 then 10 - mod11 ((- r) - 1)
            else r
;;