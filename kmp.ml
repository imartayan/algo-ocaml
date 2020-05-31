(* Algorithme de Knuth-Morris-Pratt *)

(* Calcul du tableau des bords *)
let bords x =
  let m = String.length x in
  let b = Array.make m 0 in
  let i = ref 0 in
  for j = 1 to (m-1) do
    while !i > 0 && x.[!i] <> x.[j] do
      i := b.(!i - 1)
    done;
    if x.[!i] = x.[j] then incr i;
    b.(j) <- !i
  done; b;;
(* O(m) *)

(* Algorithme de Knuth-Morris-Pratt *)
let kmp x s =
  let m = String.length x and n = String.length s in
  let b = bords x in
  let i = ref 0 in
  let rec cherche res j =
    if j = n then res
    else (
      while !i > 0 && x.[!i] <> s.[j] do
        i := b.(!i - 1)
      done;
      if x.[!i] = s.[j] then incr i;
      if !i = m then (
        i := b.(!i - 1);
        cherche ((j-m+1)::res) (j+1)
      )
      else cherche res (j+1)
    )
  in cherche [] 0;;
(* Complexité : O(m + n) *)

(* Variante *)
let kmp_bis x s =
  let m = String.length x and n = String.length s in
  let b = bords (x ^ "$" ^ s) in
  let res = ref [] in
  for k = m + 1 to m + n do
    if b.(k) = m then res := (k - 2*m)::!res
  done; !res;;

(* Exemple *)
let x = "abc";;

let s = "babbabccabcbabcbacb";;
