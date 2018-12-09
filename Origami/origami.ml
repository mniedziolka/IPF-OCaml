(* Zadanie: Origami         *)
(* Autor: Michał Niedziółka *)
(* Code review: Anita Śledź *)

let ( =. ) (x : float) (y : float) =
let e = 1e-6 and d = x -. y in ~-.e < d && d < e

type point = float * float

type kartka = point -> int

let prostokat ((a, b) : point) ((c, d) : point) ((x, y) : point) = 
  if (x <= c && x >= a && y <= d && y >= b) then 1 else 0

let kolko ((x1, y1) : point) r ((px, py) : point) =
  if ((px -. x1) *. (px -. x1) +. (py -. y1) *. (py -. y1)) <= r *. r then 1 else 0

(*  Po której stronie wektora PR jest punkt Q, -1 po lewej, 0 na, 1 po prawej*)
let strona ((x1, y1) : point) ((x2, y2) : point) ((x, y) : point)=
  let crossproduct = (x1 -. x) *. (y2 -. y) -. (x2 -. x) *. (y1 -. y) in
  if crossproduct =. 0. then 0
  else if crossproduct < 0. then 1
  else -1

let odbicie ((p1, p2) : point) ((q1, q2) : point) ((x, y) : point) =
    let wspolczynniki ((ax, ay) : point) ((bx, by) : point) =
    let a = (ay -. by) /. (ax -. bx) and b = ay -. (ax *. ((ay -. by) /. (ax -. bx))) in (a, b) in
      if p1 = q1 then ((p1 +. p1 -. x), y)
      else if p2 = q2 then (x, (p2 +. p2 -. y))
      else
          let (a, b) = wspolczynniki (p1, p2) (q1, q2) in
          let xs = ((a *. (y -. b) +. x)) /. (a *. a +. 1.0) in
          let ys = a *. xs +. b in
          let xr = 2. *. xs -. x and yr = 2. *. ys -. y in (xr, yr)

let zloz p r k q =
  if strona p r q = 0 then k q
  else if strona p r q = 1 then 0
  else let q2 = odbicie p r q in k q + k q2;;

let skladaj l k = List.fold_left (fun a (x, y) -> zloz x y a ) k l;;