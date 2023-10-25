(* Premiers pas *)

let listn1 n =
  let rec aux acc p = match p with
    | m when n<m -> acc
    | _ -> aux (p::acc) (p+1)
  in aux [] 0
;;

let length1 l =
  let rec aux acc l = match l with
    | [] -> acc
    | _::q -> aux (acc+1) q
  in aux 0 l
;;

(* Concaténation *)

let rev_append left right =
  let rec aux l1 l2 = match l1 with
    | [] -> l2
    | t::q -> aux q (t::l2)
  in aux left right
;;

let append left right = rev_append (List.rev left) right;;

(* Arbres *)

type 'a tree =
  | Nil
  | Node of 'a * 'a tree * 'a tree

let rec size1 a = match a with
  | Nil -> 0
  | Node(_,b,c) -> 1 + (size1 b) + (size1 c)
;;


let rec size' l n = match l with
  | [] -> n
  | Nil::q -> size' q n
  | Node(_,a,b)::q -> size' (a::b::q) (n+1)
;;

let size a = size' [a] 0;;

(* Listes *)

let insert x l =
  let rec aux acc l = match l with
    | [] -> List.rev_append acc [x]
    | t::q when t>x -> List.rev_append acc (x::t::q)
    | t::q when t=x -> List.rev_append acc (x::q)
    | t::q  -> aux (t::acc) q
  in aux [] l
;;

let sort l =
  let rec aux acc l' = match l' with
    | [] -> acc
    | t::q -> aux (insert t acc) q
  in aux [] l
;;

let union_sorted l1 l2 =
  let rec aux acc l1 l2 = match l1,l2 with
    | _,[] -> List.rev_append acc l1
    | [],_ -> List.rev_append acc l2
    | t::q, t'::q' when t<t' -> aux (t::acc) q l2
    | t::q, t'::q' when t'<t -> aux (t'::acc) l1 q'
    | t::q, t'::q' -> aux (t::acc) q q'
  in aux [] l1 l2
;;

let inter_sorted l1 l2 =
  let rec aux acc l1 l2 = match l1,l2 with
    | _,[] | [],_ -> List.rev acc
    | t::q, t'::q' when t<t' -> aux acc q l2
    | t::q, t'::q' when t'<t -> aux acc l1 q'
    | t::q, t'::q' -> aux (t::acc) q q'
  in aux [] l1 l2
;;