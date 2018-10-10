let reverse n =
        let rec temp x w =
                if x = 0 then w else temp (x / 10) (w * 10 + x mod 10)
        in temp n 0;;

reverse 1234;;
