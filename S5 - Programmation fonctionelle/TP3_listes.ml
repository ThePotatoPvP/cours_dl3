(* Fonctions recursives de base *)

let rec list_length l = match l with
  | [] -> 0
  | t::q -> 1 + list_length q
;;

let rec list_product l = match l with
  | [] -> 1
  | t::q -> t * (list_product q)
;;

let rec mem x l = match l with
  | [] -> false
  | t::q -> (x=t) || (mem x q)
;;

let rec list_min l = match l with
  | [] -> failwith "Pas de minimum"
  | t::[] -> t
  | t::t'::[] -> min t t'
  | t::q -> min t (list_min q)
;;

let rec last l = match l with
  | [] -> failwith "La liste est vide"
  | t::[] -> t
  | t::q -> last q
;;

let rec is_sorted l = match l with
  | [] -> true
  | t::q -> (t = list_min l) && (is_sorted q)
;;

let rec average l = match l with
  | [] -> failwith "La liste est vide"
  | t -> let rec sum l = match l with
      | [] -> 0
      | t::q -> t + (sum q)
      in (sum t) / (list_length t)
;;

let rec nth l k = match l,k with
  | t::q, 0 -> t
  | t::q, n -> nth q (n-1)
  | [], _ -> failwith "Index out of bounds" ;;

let rec range n m = match (m-n) with
  | 0 -> [m]
  | p when p>0 -> n::(range (n+1) m)
  | _ -> n::(range (n-1) m)
;;

(* Les listes et le polymorphisme *)

let rec map f l = match l with
  | [] -> []
  | t::q -> (f t)::(map f q)
;;

let rec filter p l = match l with
  | [] -> []
  | t::q when (p t) -> t::(filter p q)
  | t::q -> filter p q
;;

let rec append l1 l2 = match l1 with
  | [] -> l2
  | t::q -> t::(append q l2)
;;

let rec rev l = match l with
  | [] -> []
  | t::q -> append (rev q) [t]
;;

let rec flatten l = match l with
  | [] -> []
  | t::q -> append t (flatten q)
;;

let rec rotation_d l = match (rev l) with
  | [] -> []
  | t::q -> t::(rev q)
;;

(* Choisir des éléments au hasard *)

let rec nth k l= match l,k with
  | t::q, 0 -> t
  | [], _ -> failwith "Out of bounds"
  | t::q, o -> nth (o-1) q
;;

let rec remove_nth k l = match l,k with
  | [], _ -> []
  | t::q, 0 -> q
  | t::q, _ -> t::(remove_nth (k-1) q)
;;

let rec len l = match l with
  | [] -> 0
  | t::q -> 1 + (len q)
;;

let rec insert x l = match l with
  | [] -> [x]
  | t::q when t<x -> t::(insert x q)
  | t::q when t>x -> x::l
  | t::q -> l
;;

let choose l =
  nth (Random.int (le l)) l
;;

let choose_elements l n =
  let e = ref l in
  for i = 1 to ((le l)-n) do
    e := remove_nth (Random.int (le !e)) !e
  done;
  !e
;;

let choose_sublist l n =
  choose_elements l n
;;

(* Différents tris *)

let rec insert x l = match l with
  | [] -> [x]
  | t::q when t<x -> t::(insert x q)
  | t::q when t>x -> x::l
  | t::q -> l
;;

let rec sort l = match l with
  | [] -> []
  | t::q -> insert t (sort q)
;;

let rec mem_sorted x l = match l with
  | [] -> false
  | t::q when t>x -> false
  | t::q -> (x=t) || (mem_sorted x q)
;;

let rec union_sorted l1 l2 = match l1,l2 with
  | _,[] -> l1
  | [],_ -> l2
  | t::q,t'::q' when t<t' -> t::(union_sorted q l2)
  | t::q,t'::q' when t=t' -> t::(union_sorted q q')
  | _,t::q -> t::(union_sorted l1 q)
;;

let rec inter_sorted l1 l2 = match l1,l2 with
  | _,[] | [],_ -> []
  | t::q,t'::q' when t<t' -> inter_sorted q l2
  | t::q,t'::q' when t=t' -> t::(inter_sorted q q')
  | t::q,t'::q' -> inter_sorted l1 q'
;;

let rec filter p l = match l with
  | [] -> []
  | t::q when (p t) -> t::(filter p q)
  | t::q -> filter p q
;;


let rec quicksort l = match l with
  | [] -> l
  | [t] -> l
  | t::q ->
      let inf = (filter (fun x -> x<=t) q) in
      let sup = (filter (fun x -> x>t) q) in
      (quicksort inf) @ [t] @ (quicksort sup)
;;