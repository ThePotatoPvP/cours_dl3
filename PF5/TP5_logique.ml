type formula =
  | Prop of string
  | Neg of formula
  | And of formula * formula
  | Or of formula * formula

(* Calcul propositionel *)

let rec string_of_formula f = match f with
  | Prop(a) -> a
  | Neg(g) -> "Neg "^(string_of_formula g)
  | And(a,b) -> "("^(string_of_formula a)^" And "^(string_of_formula b)^")"
  | Or(a,b) -> "("^(string_of_formula a)^" Or "^(string_of_formula b)^")"
;;

let rec list_of_props f = match f with
  | Prop(a) -> [a]
  | Neg(a) -> list_of_props a
  | And(a,b)| Or(a,b) -> union_sorted (list_of_props a) (list_of_props b)
;;

let rec eval_formula f l = match f with
  | Prop(a) -> List.assoc a l
  | Neg(a) -> not (eval_formula a l)
  | And(a,b) -> (eval_formula a l) && (eval_formula b l)
  | Or(a,b) -> (eval_formula a l) || (eval_formula b l)
;;

(* Formules satisfiables et tautologies *)

let add_to_all x ll = List.map (fun l -> x::l) ll;;

let rec interpretations_props l = match l with
  | [] -> [[]]
  | t::q -> (add_to_all (t, true) (interpretations_props q))@
            (add_to_all (t, false) (interpretations_props q))
;;

let interpretations f = interpretations_props (list_of_props f);;

let satisfiable f = List.exists (fun l -> (eval_formula f l)) (interpretations f);;

let tautology f = List.for_all (fun l -> (eval_formula f l)) (interpretations f);;

(* Conséquence et équivalence *)

let is_consequence f g = List.for_all (fun l -> (eval_formula g l))
    (List.filter (fun l -> (eval_formula f l)) (interpretations (Or(g, f))));;

let are_equivalent f g = (is_consequence f g) && (is_consequence g f) ;;