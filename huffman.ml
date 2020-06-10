(* Codage de Huffman *)

(* On commence par définir les structures de données dont on aura besoin *)
(* Dictionnaire avec un arbre binaire de recherche *)
type ('a,'b) dict =
  |Nil
  |N of 'a * 'b * ('a, 'b) dict * ('a, 'b) dict;;

let rec taille_dict = function
  |Nil -> 0
  |N(_, _, g, d) -> 1 + taille_dict g + taille_dict d;;

let rec find_dict a x = match a with
  |Nil -> failwith "élément introuvable"
  |N(x', y', g, d) -> if x = x' then y'
    else if x < x' then find_dict g x
    else find_dict d x;;

let rec ajoute_dict a x y = match a with
  |Nil -> N(x, y, Nil, Nil)
  |N(x', y', g, d) -> if x = x' then N(x, y, g, d)
    else if x < x' then N(x', y', ajoute_dict g x y, d)
    else N(x', y', g, ajoute_dict d x y);;

(* Parcours préfixe qui appelle f pour chaque noeud *)
let rec iter_dict a f = match a with
  |Nil -> ()
  |N(x, y, g, d) -> f x y; iter_dict g f; iter_dict d f;;

(* On va utiliser un dictionnaire pour compter les occurrences d'un caractère *)
let rec incr_compteur a x = match a with
  |Nil -> N(x, 1, Nil, Nil)
  |N(c, n, g, d) -> if x = c then N(x, n+1, g, d)
    else if x < c then N(c, n, incr_compteur g x, d)
    else N(c, n, g, incr_compteur d x);;

(* Arbre binaire pondéré étiquetté par des caractères *)
type arbre_pondere =
  |F of char * int
  |N of arbre_pondere * arbre_pondere * int;;

(* Chaque noeud contient son propre poids *)
let poids = function
  |N(_,_,p) |F(_,p) -> p;;

(* Comparaison utilisée pour le tas *)
let (<<) a b = poids a < poids b;;

(* Tas min d'arbres pondérés *)
type tas = {
  tab: arbre_pondere array;
  mutable taille: int
};;

let init_tas n = {
  tab = Array.make n (F('$', 0));
  taille = 0
};;

let swap t i j = let tmp = t.tab.(i) in
  t.tab.(i) <- t.tab.(j); t.tab.(j) <- tmp;;

let rec remonte t i =
  let p = (i - 1) / 2 in
  if i > 0 && t.tab.(i) << t.tab.(p) then (
    swap t i p; remonte t p
  );;

let ajoute_tas t a =
  let i = t.taille in
  t.tab.(i) <- a; t.taille <- i + 1;
  remonte t i;;

let rec descend t i =
  let g = 2*i+1 and d = 2*i+2 in
  if d < t.taille && t.tab.(d) << t.tab.(i) then
    if t.tab.(g) << t.tab.(d) then (
      swap t i g; descend t g
    ) else (swap t i d; descend t d)
  else if g < t.taille && t.tab.(g) << t.tab.(i) then (
    swap t i g; descend t g
  );;

let prend_min t =
  if t.taille = 0 then failwith "tas vide";
  let n = t.taille - 1 in
  swap t 0 n; t.taille <- n;
  descend t 0; t.tab.(n);;

(* Fonctionnement du codage de Huffman *)
(* On commence par compter le nombre d'occurrences de chaque caractère *)
(* On créé un arbre pour chaque caractère et on l'ajoute au tas min *)
(* On fusionne les deux arbres de poids minimal jusqu'à obtenir un seul arbre *)

let compter_occ s =
  let n = String.length s in
  let d = ref Nil in
  for i = 0 to (n-1) do
    d := incr_compteur !d s.[i]
  done; !d;;

let tas_occ s =
  let d = compter_occ s in
  let n = taille_dict d in
  let t = init_tas n in
  let f c k = ajoute_tas t (F(c, k)) in
  iter_dict d f; t;;

let arbre_huffman s =
  let t = tas_occ s in
  while t.taille > 1 do
    let a = prend_min t in
    let b = prend_min t in
    ajoute_tas t (N(a, b, poids a + poids b))
  done; t.tab.(0);;

let table_huffman s =
  let a = arbre_huffman s in
  let d = ref Nil in
  let rec parcours pref = function
    |F(c, _) -> d := ajoute_dict !d c pref
    |N(g, d, _) -> (
        parcours (pref ^ "0") g;
        parcours (pref ^ "1") d
      )
  in parcours "" a; !d;;

(* Encodage et décodage *)
let encode s t =
  let n = String.length s in
  let l = ref [] in
  for i = (n-1) downto 0 do
    l := (find_dict t s.[i])::!l
  done; !l;;

let reverse_dict d =
  let r = ref Nil in
  let f x y = r := ajoute_dict !r y x in
  iter_dict d f; !r;;

let decode l r =
  let rec aux s = function
    |[] -> s
    |y::t -> aux (s ^ (Char.escaped (find_dict r y))) t
  in aux "" l;;

(* Exemple *)
let s = "computer science is no more about computers than astronomy is about telescopes";;
let t = table_huffman s;;
let l = encode s t;;
let r = reverse_dict t;;
