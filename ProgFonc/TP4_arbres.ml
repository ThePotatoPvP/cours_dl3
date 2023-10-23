type int_tree =
  | Nil
  | Node of int * int_tree * int_tree

(* Arbres Binaires *)

let rec size a = match a with
  | Nil -> 0
  | Node(b, l, r) -> 1 + (size l) + (size r)
;;

let rec depth a = match a with
  | Nil -> 0
  | Node(b, l, r) -> 1 + (max (depth l) (depth r))
;;

let rec sum a = match a with
  | Nil -> 0
  | Node(b, l, r) -> b + (sum l) + (sum r)
;;

let rec contains x a = match a with
  | Nil -> false
  | Node(b, l, r) -> (x = b) || (contains x l) || (contains x r)
;;

let rec elements a = match a with
  | Nil -> []
  | Node(b, l, r) -> (elements l)@[b]@(elements r)
;;

let rec perfect a = match a with
  | Nil -> true
  | Node(_,l,r) -> (depth l) = (depth r) && (perfect l) && (perfect r)
;;

(* Arbres binaire de recherche *)
let rec contains_bst x a = match a with
  | Nil -> false
  | Node(v, l, r) when v < x -> contains_bst x r
  | Node(v, l, r) when v > x -> contains_bst x l
  | _ -> true
;;

let rec add_bst x a = match a with
  | Nil -> Node(x, Nil, Nil)
  | Node(v, l, r) when v < x -> Node(v, l, (add_bst x r))
  | Node(v, l, r) when v > x -> Node(v, (add_bst x l), r)
  | Node(v, l, r) -> Node(v, l, r)
;;

let rec bst_of_list l = match l with
  | [] -> Nil
  | t::q -> add_bst t (bst_of_list q)
;;

let rec bst_of_list_opt l =
  let rec mkl l = match l with
    | [] -> []
    | _ ->
        let mid = List.nth l ((List.length l)/2) in
        [mid]@(mkl (List.filter (fun x-> x<mid) l))@(mkl (List.filter (fun x-> x>mid) l))
  in bst_of_list (List.rev (mkl l))
;;

let rec is_bst a =
  let rec aux a mi ma = match a with
    | Nil -> true
    | Node(v, Nil, Nil) -> (v>mi && v<ma)
    | Node(v, Node(v', l, r), Nil) ->
        (v>mi && v<ma) && (v'<v) && aux (Node(v', l, r)) mi v
    | Node(v, Nil, Node(v', l, r)) ->
        (v>mi && v<ma) && (v<v') && aux (Node(v', l, r)) v ma
    | Node(v, Node(v1, l1, r1), Node(v2, l2, r2)) ->
        aux (Node(v, Node(v1, l1, r1), Nil)) mi ma &&
        aux (Node(v, Nil, Node(v2, l2, r2))) mi ma
  in aux a (-1000) 1000
;;

(* Arbres polymorphes *)
type 'a tree =
  | Nil
  | Node of 'a * 'a tree * 'a tree

let rec fold_tree fn vf a =
  match a with
  | Nil -> vf
  | Node (n, g, d) -> fn n (fold_tree fn vf g) (fold_tree fn vf d)
;;

let rec forall_labels p a = match a with
  | Nil -> true
  | Node(v, l, r) -> (p v) && (forall_labels p l) && (forall_labels p r)
;;

let is_uniform x a = forall_labels (fun k-> k=x) a;;

let rec forall_subtrees pn a = match a with
  | Nil -> true
  | Node(v, l, r) -> pn v l r && (forall_subtrees pn l) && (forall_subtrees pn r)
;;

let is_right_comb a = forall_subtrees (fun a b c -> b=Nil) a;;

let sum a = fold_tree (fun v l r -> v+l+r) 0 a;;

let map_tree f a = fold_tree (fun v l r -> Node((f v), l, r)) Nil a;;