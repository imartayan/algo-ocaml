(* Algorithme de Prim *)

(* Type poids *)
type poids = P of int | Inf;;

let (<<) a b = match a, b with
  |P x, P y -> x < y
  |Inf, _ -> false
  |_, Inf -> true;;

(* Tas min modifiable *)
type 'a tas = {
  tab : 'a array;
  som : int array;
  ind : int array;
  mutable taille : int
};;
(* som.(i) est le sommet stocké en i
   ind.(u) est l'indice du sommet u *)

let init n default = {
  tab = Array.make n default;
  som = Array.make n (-1);
  ind = Array.make n (-1);
  taille = 0
};;

let mem u t = t.ind.(u) <> -1;;

let swap t i j =
  let tmp = t.tab.(i) in
  t.tab.(i) <- t.tab.(j); t.tab.(j) <- tmp;
  let u = t.som.(i) and v = t.som.(j) in
  t.som.(i) <- v; t.som.(j) <- u;
  t.ind.(v) <- i; t.ind.(u) <- j;;

(* Remonter le ième élément du tas *)
let rec remonte t i =
  if i >= 1 && t.tab.(i) << t.tab.((i-1)/2) then (
    swap t i ((i-1)/2);
    remonte t ((i-1)/2)
  );;

(* Descendre le ième élément du tas *)
let rec descend t i =
  if 2*i+2 < t.taille && t.tab.(2*i+2) << t.tab.(i) then
    if t.tab.(2*i+1) << t.tab.(2*i+2) then (
      swap t i (2*i+1);
      descend t (2*i+1)
    )
    else (
      swap t i (2*i+2);
      descend t (2*i+2)
    )
  else if 2*i+1 < t.taille && t.tab.(2*i+1) << t.tab.(i) then (
    swap t i (2*i+1);
    descend t (2*i+1)
  );;

(* Ajouter le sommet u de valeur x *)
let add u x t =
  if t.taille = Array.length t.tab then failwith "tas plein"
  else (
    t.tab.(t.taille) <- x;
    t.som.(t.taille) <- u;
    t.ind.(u) <- t.taille;
    remonte t t.taille;
    t.taille <- t.taille + 1
  );;

(* Retirer le minimum du tas et le renvoyer *)
let take_min t =
  if t.taille = 0 then failwith "tas vide"
  else if t.taille = 1 then (
    let u = t.som.(0)
    and x = t.tab.(0) in
    t.taille <- 0;
    t.som.(0) <- -1;
    t.ind.(u) <- -1;
    (u, x)
  )
  else (
    let u = t.som.(0)
    and x = t.tab.(0) in
    t.taille <- t.taille - 1;
    swap t 0 t.taille;
    t.som.(t.taille) <- -1;
    t.ind.(u) <- -1;
    descend t 0;
    (u, x)
  );;

(* Remplacer la valeur du sommet u par x *)
let update u x t =
  t.tab.(t.ind.(u)) <- x;
  remonte t t.ind.(u);
  descend t t.ind.(u);;

(* Algorithme de Prim *)
let prim g m =
  let n = Array.length g in
  let dist = Array.make n Inf
  and pred = Array.make n 0
  and fini = Array.make n false
  and tas = init n Inf in
  let traite i j =
    if not fini.(j) then (
      if m.(i).(j) << dist.(j) then (
        dist.(j) <- m.(i).(j);
        pred.(j) <- i;
        if mem j tas then update j dist.(j) tas
      );
      if not (mem j tas) then add j dist.(j) tas
    ) in
  dist.(0) <- P 0;
  add 0 dist.(0) tas;
  for _ = 1 to n do
    let i, _ = take_min tas in
    fini.(i) <- true;
    List.iter (traite i) g.(i)
  done; pred;;
(* Complexité : O((n + p) log n) *)

(* Exemple *)
let g = [|
  [1;2;3];
  [0;4;5];
  [0;3;6];
  [0;2;4;7];
  [1;3;5;8];
  [1;4;9];
  [2;7;10];
  [3;6;8;10];
  [4;7;9;11];
  [5;8;11];
  [6;7;11];
  [8;9;10]
|];;

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
