let buduj n = 
        let rec pom l res = 
                match l with
                [] -> [] :: res
                | h :: t -> pom t (l :: res)
        in pom n []
;;
