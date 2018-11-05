let rec buduj n =
        if n = 0 then []
        else n :: buduj (n - 1)
;;

let rec budujogonowo n =
        let rec pom n l =
                if n = 0 then l
                else pom (n - 1) (n :: l)
        in pom n []
;;

let sum li = List.fold_left (+) 0 li;;
let prod li = List.fold_left ( * ) 1 li;;

let len li = List.fold_left (fun x _ -> x + 1) 0 li;;
