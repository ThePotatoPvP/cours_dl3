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

(* Forme normale conjonctive *)

let rec desc_neg f = match f with
  | Prop(a) -> Prop(a)
  | Neg(Prop(a)) -> Neg(Prop(a))
  | Neg(Neg(g)) -> desc_neg g
  | Neg(Or(a,b)) -> And((desc_neg (Neg(a))), (desc_neg (Neg(b))))
  | Neg(And(a,b)) -> Or((desc_neg (Neg(a))), (desc_neg (Neg(b))))
  | Or(a,b) -> Or((desc_neg a), (desc_neg b))
  | And(a,b) -> And((desc_neg a), (desc_neg b))
;;

let rec is_curated f = match f with
  | Or(_,And(_,_)) | Or(And(_,_),_) -> false
  | And(a,b)| Or(a,b) -> (is_curated a) && (is_curated b)
  | _ -> true
;;

let rec contains_and f = match f with
  | And _ -> true
  | Or (a, b) -> contains_and a || contains_and b
  | Neg a -> contains_and a
  | Prop _ -> false


let rec distribute_or_over_and f = match f with
  | And (a, b) ->
      let dist_a = distribute_or_over_and a in
      let dist_b = distribute_or_over_and b in
      And (dist_a, dist_b)
  | Or (And (a, b), c) ->
      And (Or (distribute_or_over_and a, distribute_or_over_and c), Or (distribute_or_over_and b, distribute_or_over_and c))
  | Or (a, And (b, c)) ->
      And (Or (distribute_or_over_and a, distribute_or_over_and b), Or (distribute_or_over_and a, distribute_or_over_and c))
  | Or (a, b) when not (contains_and a || contains_and b) ->
      Or (distribute_or_over_and a, distribute_or_over_and b)
  | Or (a, b) ->
      Or (distribute_or_over_and a, distribute_or_over_and b)
  | Neg a -> Neg (distribute_or_over_and a)
  | Prop a -> Prop a

let rec desc_or f =
  let simplified = distribute_or_over_and f in
  if simplified = f then
    f
  else
    desc_or simplified
;;

let cnf f = desc_or (desc_neg f) ;;

