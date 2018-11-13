type 'a queue = 
    | Null
    | Node of 'a queue * 'a * 'a queue * int;;

let empty = Null

exception Empty;;

let is_empty a = 
    a = Null;;

let getrdepth a = 
    match a with
    | Null -> -1
    | Node (, , , d) -> d;;

let rec join a b = 
    match a, b with
    | Null,  -> b
    | , Null -> a
    | Node (la, va, ra, ha), Node (lb, vb, rb, hb) ->
        if va > vb then join b a else
        let new_subtree = join ra b in
        let ldepth = get_rdepth la and newdepth = get_rdepth new_subtree in
        if ldepth < newdepth then
            Node (la, va, new_subtree, newdepth + 1)
        else
            Node (new_subtree, va, la, ldepth + 1);;

let add e q = 
    let aux_queue = Node (Null, e, Null, 1) in
    join aux_queue q;;

let deletemin q = 
    match q with
    | Null -> raise Empty
    | Node (l, v, r, ) ->
        (v, (join l r));;