(* Autor: Michał Niedziółka *)
(* Code review: Kuba Organa *)

type inter = int * int

type t = Empty | Node of t * inter * t * int * int

exception Broken

let node s = 
    match s with 
    |  (_, v, _, _, _) -> v
    | Empty -> failwith "Empty set passed to function: value"

let height s =
    match s with
    | Set (_, _, _, h, _) -> h 
    | Empty -> 0

let maxsum x y = if x >= max_int + y then max_int else x + y

let empty = Empty
let is_empty s = s = Empty

let make l k r = Node (l, k, r, max (height l) (height r) + 1)

let bal l k r =
  let hl = height l in
  let hr = height r in
  if hl > hr + 2 then
    match l with
    | Node (ll, lk, lr, _, _) ->
        if height ll >= height lr then make ll lk (make lr k r)
        else
          (match lr with
          | Node (lrl, lrk, lrr, _, _) ->
              make (make ll lk lrl) lrk (make lrr k r)
          | Empty -> assert false)
    | Empty -> assert false
  else if hr > hl + 2 then
    match r with
    | Node (rl, rk, rr, _, _) ->
        if height rr >= height rl then make (make l k rl) rk rr
        else
          (match rl with
          | Node (rll, rlk, rlr, _, _) ->
              make (make l k rll) rlk (make rlr rk rr)
          | Empty -> assert false)
    | Empty -> assert false
  else make l k r

let rec min_elt = function
  | Node (Empty, k, _, _, _) -> k
  | Node (l, _, _, _, _) -> min_elt l
  | Empty -> raise Not_found

let rec remove_min_elt = function
  | Node (Empty, _, r, _, _) -> r
  | Node (l, v, r, _, _) -> bal (remove_min_elt l) v r
  | Empty -> invalid_arg "PSet.remove_min_elt"

let rec max_elt = function
  | Node (_, v, Empty, _, _) -> v
  | Node (_, _, r, _, _) -> max_elt r
  | Empty -> raise Not_found

let rec remove_max_elt = function
  | Node (l, _, Empty, _, _) -> l
  | Node (l, v, r, _, _) -> bal l v (remove_max_elt r)
  | Empty -> invalid_arg "PSet.remove_max_elt"


let iter f s =
  let rec loop = function
    | Empty -> ()
    | Node (l, v, r, _, _) -> loop l; f v; loop r
  in
  loop s

let fold f s acc =
  let rec loop acc = function
    | Empty -> acc
    | Node (l, v, r, _, _) -> loop (f v (loop acc l)) r
  in
  loop acc s

let elements s =
  let rec loop acc = function
    | Empty -> acc
    | Node(l, v, r, _, _) -> loop (v :: loop acc r) l in
  loop [] s

  (* Porownoje 2 przedzialy *)
(* Warunek: sa rozlaczne *)
let cmp_normal (a, b) (c, d) =
  if a > d then 1
  else if b < c then -1
  else raise Broken

(* Dodaje przedzial do drzewa *)
(* Warunek: przedzial ten jest rozlaczny z kazdym obecnym aktualnie w drzewie *)
let rec add_normal x = function
  | Node (l, v, r, _, _) ->
    let c = cmp_normal x v in
    if c < 0 then
      let nl = add_normal x l in
      bal nl v r
    else
      let nr = add_normal x r in
      bal l v nr
  | Empty -> Node (Empty, x, Empty, 1, elem_count x)

let rec join l v r =
  match (l, r) with
  | (Empty, _) -> add_normal v r
  | (_, Empty) -> add_normal v l
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
            if x > a then add_normal (a, x - 1) l
            else l
          in
          let new_r =
            if x < b then add_normal (x + 1, b) r
            else r
          in (new_l, true, new_r)
        else if c < 0 then
          let (ll, pres, rl) = loop x l in (ll, pres, join rl (a, b) r)
        else
          let (lr, pres, rr) = loop x r in (join l (a, b) lr, pres, rr)
  in loop x s

(* Dodaje przedzial (x, y) do s*)
let add (x, y) s =
  if is_empty s then add_normal (x, y) s else
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