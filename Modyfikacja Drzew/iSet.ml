(* Modyfikacja drzew *)
(* Autor: Michał Niedziółka *)
(* Code review: Jakub Organa *)

(* kolejno: lewe poddrzewo * wartosc(przedzial) * prawe * wysokosc * liczba elementow *)

type interval = int * int
type t = 
  | Empty 
  | Node of t * interval * t * int * int

let empty = Empty
let is_empty s = s = Empty

let node x = 
  match x with
  | Node (_, v, _, _, _) -> v
  | Empty -> failwith "Empty set passed to function: value"

let height x = 
  match x with
  | Node (_, _, _, h, _) -> h
  | Empty -> 0

let count x =
  match x with
  | Node (_, _, _, _, c) -> c
  | Empty -> 0

(* Sumuje liczby elementow, przy czym wynik nie przekracza max_int *)
let maxsum x y = if x >= max_int + y then max_int else x + y

(* Zwraca liczbe elementow w przedziale (a, b) *)
let elem_count (a, b) =
  let neg_spec x = if x = min_int then max_int else -x in
  if (a > 0 && b > 0) then b - a + 1
  else if (a < 0 && b < 0) then
    if a = min_int then maxsum (b + neg_spec a + 1) 1
    else b - a + 1
  else maxsum (maxsum (neg_spec a) b) 1

(* Tworzy nowe drzewo z danego lewego, prawego poddrzewa i nowego korzenia *)
(* Warunek poczatkowy: oba drzewa sa drzewami AVL, roznica wysokosci = 1, korzen do nich pasuje *)
let make l k r =
  let c = maxsum (maxsum (count l) (count r)) (elem_count k) in
  Node (l, k, r, max (height l) (height r) + 1, c)

let make_leaf v = Node (Empty, v, Empty, 1, elem_count v)

(* PRZEKLEJONE Z pSetu *)
let bal l k r =
let hl = height l in
let hr = height r in
if hl > hr + 2 then
  match l with
  | Node (ll, lk, lr, _, _) ->
      if height ll >= height lr then make ll lk (make lr k r)
      else (
      match lr with
        | Node (lrl, lrk, lrr, _, _) -> make (make ll lk lrl) lrk (make lrr k r)
        | Empty -> assert false)
  | Empty -> assert false
else if hr > hl + 2 then
  match r with
  | Node (rl, rk, rr, _, _) ->
      if height rr >= height rl then make (make l k rl) rk rr
      else (
      match rl with
        | Node (rll, rlk, rlr, _, _) -> make (make l k rll) rlk (make rlr rk rr)
        | Empty -> assert false)
  | Empty -> assert false
else make l k r

let rec min_elt = function
  | Node (Empty, k,_,_,_) -> k
  | Node (l,_,_,_,_) -> min_elt l
  | Empty -> raise Not_found

let rec remove_min_elt = function
  | Node (Empty,_,r,_,_) -> r
  | Node (l,k,r,_,_) -> bal (remove_min_elt l) k r
  | Empty -> invalid_arg "PSet.remove_min_elt"

let rec max_elt = function
  | Node (_,k,Empty,_,_) -> k
  | Node (_,_,r,_,_) -> max_elt r
  | Empty -> raise Not_found

let rec remove_max_elt = function
  | Node (l,_,Empty,_,_) -> l
  | Node (l,k,r,_,_) -> bal l k (remove_max_elt r)
  | Empty -> invalid_arg "PSet.remove_max_elt"


(* DOTĄD SĄ KODY PRZEKLEJONE Z pSetu *)

exception Invalid_args

(* Dodaję do drzewa przedział który jest rozłączny z pozostałymi na drzewie *)
let rec add_disjoint x = function
  | Node (l, v, r, _, _) ->
    let c = fst(x) > snd(v) in
    if c then let node_right = add_disjoint x r 
      in bal l v node_right
    else let node_left = add_disjoint x l 
      in bal node_left v r
  | Empty -> make_leaf x


(* join wzięty z pSet *)
let rec join l v r =
  match (l, r) with
  | (Empty, _) -> add_disjoint v r
  | (_, Empty) -> add_disjoint v l
  | (Node(ll, lv, lr, lh, _), Node(rl, rv, rr, rh, _)) ->
      if lh > rh + 2 then bal ll lv (join lr v r) else
      if rh > lh + 2 then bal (join l v rl) rv rr else
      make l v r

(* Porownuje wartosc z przedzialem *)
let cmp_val_set (x, y) v =
  if v < x then -1
  else if v > y then 1
  else 0 (* v zawiera sie w (x, y) *)

(* Czy w s istnieje przedzial, w ktorym zawiera sie x? *)
let mem x s =
  let rec loop = function
    | Node (l, k, r, _, _) ->
        let c = cmp_val_set k x in
        c = 0 || loop (if c < 0 then l else r)
    | Empty -> false
  in
  loop s

let split x s =
  let rec loop x = function
    | Empty ->
        (Empty, false, Empty)
    | Node (l, (a, b), r, _, _) ->
        let c = cmp_val_set (a, b) x in
        if c = 0 then
          let new_l =
            if x > a then add_disjoint (a, x - 1) l
            else l
          in
          let new_r =
            if x < b then add_disjoint (x + 1, b) r
            else r
          in
          (new_l, true, new_r)
        else if c < 0 then
          let (ll, pres, rl) = loop x l in (ll, pres, join rl (a, b) r)
        else
          let (lr, pres, rr) = loop x r in (join l (a, b) lr, pres, rr)
  in
  loop x s

(* Dodaje przedzial (x, y) do s*)
let add (x, y) s =
  if is_empty s then add_disjoint (x, y) s else
  let (left, _, _) = split x s in
  let (_, _, right) = split y s in

  let (left, a) =
    if left = empty then (left, x)
    else
      let (pl, kl) = max_elt left in
      if x = kl + 1 then (remove_max_elt left, pl)
      else (left, x)
  in

  let (right, b) =
    if right = empty then (right, y)
    else
      let (pr, kr) = min_elt right in
      if y = pr - 1 then (remove_min_elt right, kr)
      else (right, y)
  in
  join left (a, b) right

(* Usuwa przedzial (x, y) z s*)
let remove (x, y) s =
  if is_empty s then empty else
  let (left, _, _) = split x s in
  let (_, _, right) = split y s in
  match (left, right) with
  | (Empty, _) -> right
  | (_, Empty) -> left
  | _ -> join left (min_elt right) (remove_min_elt right)

let iter f s =
  let rec loop = function
    | Empty -> ()
    | Node (l, k, r, _, _) -> loop l; f k; loop r
  in
  loop s

let fold f s acc =
  let rec loop acc = function
    | Empty -> acc
    | Node (l, k, r, _, _) -> loop (f k (loop acc l)) r
  in
  loop acc s

let elements s =
  let rec loop acc = function
    | Empty -> acc
    | Node(l, k, r, _, _) -> loop (k :: loop acc r) l in
  loop [] s

(* Zwraca liczbe elementow (liczb calkowitych), ktore sa <= n *)
(* Nalezacych do sumy wszystkich przedzialow w s *)
let below n s =
  match s with
  | Empty -> 0
  | Node (_, _, _, _, c) ->
      if n = max_int then c
      else
        let set = remove (n + 1, max_int) s in
        match set with
        | Empty -> 0
        | Node (_, _, _, _, x) -> x