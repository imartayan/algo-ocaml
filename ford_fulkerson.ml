(* Algorithme de Ford-Fulkerson *)

let valeur_flot f =
  let n = Array.length f and v = ref 0 in
  for i = 0 to n-1 do
    v := !v + f.(0).(i)
  done; !v;;

let graphe_residuel c f =
  let n = Array.length c in
  let g = Array.make_matrix n n 0 in
  for i = 0 to (n-1) do
    for j = 0 to (n-1) do
      if c.(i).(j) > 0 then (
        g.(i).(j) <- c.(i).(j) - f.(i).(j);
        g.(j).(i) <- f.(i).(j)
      )
    done
  done; g;;

let chemin_augmentant g =
  let n = Array.length g in
  let djvu = Array.make n false in
  let rec chemin_depuis i =
    if i = n-1 then [i]
    else if djvu.(i) then []
    else (
      djvu.(i) <- true;
      let ch_i = ref [] in
      for j = 0 to (n-1) do
        if g.(i).(j) > 0 then
          let ch_j = chemin_depuis j in
          if ch_j <> [] then
            ch_i := i::ch_j
      done; !ch_i
    )
  in chemin_depuis 0;;

let rec poids_min_chemin g = function
  |[i;j] -> g.(i).(j)
  |i::j::t -> min g.(i).(j) (poids_min_chemin g (j::t))
  |_ -> 0;;

let augmente_flot c g ch f =
  let m = poids_min_chemin g ch in
  let rec parcours = function
    |i::j::t -> if c.(i).(j) > 0
      then f.(i).(j) <- f.(i).(j) + m
      else f.(j).(i) <- f.(j).(i) - m;
      parcours (j::t)
    |_ -> ()
  in parcours ch;;

(* Algorithme de Ford-Fulkerson *)
let flot_max c =
  let n = Array.length c in
  let f = Array.make_matrix n n 0 in
  let g = ref (graphe_residuel c f) in
  let ch = ref (chemin_augmentant !g) in
  while !ch <> [] do
    augmente_flot c !g !ch f;
    g := graphe_residuel c f;
    ch := chemin_augmentant !g
  done; f;;

(* Exemple *)
let c = [|
  [|0; 20; 8; 0|];
  [|0; 0; 25; 10|];
  [|0; 0; 0; 19|];
  [|0; 0; 0; 0|]
|];;
