type 'a queue =
  | Node of 'a * int * 'a queue * 'a queue
  | Null

exception Empty

let value n = 
    match n with 
        | Null -> raise Empty
        | Node(v,_,_,_) -> v
let height n =
    match n with 
        | Null -> 0
        | Node(_,h,_,_) -> h
let left n =
    match n with 
        | Null -> raise Empty
        | Node(_,_,l,_) -> l
let right n =
    match n with 
        | Null -> raise Empty
        | Node(_,_,_,r) -> r


let empty = Null
let leaf n = Node(n, 1, Null, Null)


let is_empty n = 
    match n with
        | Null -> true
        | Node(_,_,_,_) -> false

let rec join a b =
    if is_empty a then b
    else if is_empty b then a
    else if value a > value b then join b a
    else let c = join (right a) b 
    in if height (left a) < height c
    then Node (value a, (height a) + 1, c, left a)
    else Node (value a, (height c) + 1, left a, c)

let add e q = join (leaf e) q

let delete_min q = 
    if is_empty q then raise Empty
    else (value q, join (left q) (right q)) 
