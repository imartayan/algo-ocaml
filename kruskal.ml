(* Algorithme de Kruskal *)

(* Type poids *)
type poids = P of int | Inf;;

let (<<) a b = match a, b with
  |P x, P y -> x < y
  |Inf, _ -> false
  |_, Inf -> true;;

(* Tri fusion des arêtes *)
let rec fusion m l1 l2 = match l1, l2 with
  |[], _ -> l2
  |_, [] -> l1
  |(i, j)::t1, (k, l)::t2 -> if m.(i).(j) << m.(k).(l)
    then (i, j)::(fusion m t1 l2)
    else (k, l)::(fusion m l1 t2);;

let rec split = function
  |[] -> [], []
  |h::t -> let l1, l2 = split t in h::l2, l1;;

let rec tri m = function
  |[] -> []
  |[x] -> [x]
  |l -> let l1, l2 = split l in
    fusion m (tri m l1) (tri m l2);;

let tri_aretes m =
  let n = Array.length m in
  let l = ref [] in
  for i = 0 to (n-1) do
    for j = (i+1) to (n-1) do
      if m.(i).(j) << Inf then l := (i, j)::!l
    done
  done; tri m !l;;
(* Complexité : O(p log p) *)

(* Union-find par compression *)
type unionfind = {t : int array; mutable n : int};;

let init n = {t = Array.init n (fun x -> x); n = n};;

let rec find u a =
  let p = u.t.(a) in
  if p = a then a
  else (
    let r = find u p in
    u.t.(a) <- r; r
  );;

let union u a b =
  let p = find u a and q = find u b in
  if p <> q then (
    u.t.(q) <- p;
    u.n <- u.n - 1
  );;

(* Algorithme de Kruskal *)
let kruskal m =
  let n = Array.length m in
  let u = init n in
  let rec aux r = function
    |[] -> r
    |(i, j)::t -> if u.n = 1 then r
      else if find u i = find u j then aux r t
      else (
        union u i j;
        aux ((i, j)::r) t
      )
  in aux [] (tri_aretes m);;
(* Complexité : O(p log p) *)

(* Exemple *)
let m = [|
  [|Inf;P 3;P 5;P 4;Inf;Inf;Inf;Inf;Inf;Inf;Inf;Inf;Inf|];
  [|P 3;Inf;Inf;Inf;P 3;P 6;Inf;Inf;Inf;Inf;Inf;Inf;Inf|];
  [|P 5;Inf;Inf;P 2;Inf;Inf;P 4;Inf;Inf;Inf;Inf;Inf;Inf|];
  [|P 4;Inf;P 2;Inf;P 1;Inf;Inf;P 5;Inf;Inf;Inf;Inf;Inf|];
  [|Inf;P 3;Inf;P 1;Inf;P 2;Inf;Inf;P 4;Inf;Inf;Inf;Inf|];
  [|Inf;P 6;Inf;Inf;P 2;Inf;Inf;Inf;Inf;P 5;Inf;Inf;Inf|];
  [|Inf;Inf;P 4;Inf;Inf;Inf;Inf;P 3;Inf;Inf;P 6;Inf;Inf|];
  [|Inf;Inf;Inf;P 5;Inf;Inf;P 3;Inf;P 6;Inf;P 7;Inf;Inf|];
  [|Inf;Inf;Inf;Inf;P 4;Inf;Inf;P 6;Inf;P 3;Inf;P 5;Inf|];
  [|Inf;Inf;Inf;Inf;Inf;P 5;Inf;Inf;P 3;Inf;Inf;P 9;Inf|];
  [|Inf;Inf;Inf;Inf;Inf;Inf;P 6;P 7;Inf;Inf;Inf;Inf;P 8|];
  [|Inf;Inf;Inf;Inf;Inf;Inf;Inf;Inf;P 5;P 9;Inf;P 8;Inf|];
|];;
