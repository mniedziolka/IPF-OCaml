 let pierwiastki n =
   let rec spr p =
     if p >= n-1 then false
     else if (p * p) mod n = 1 then true
     else spr (p+1)
   in
     spr 2;;