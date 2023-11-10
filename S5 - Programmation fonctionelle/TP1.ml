(* Expressions arithmétiques *)

let square x = x*x;;

let perimeter r =
  let rec pi = 3.1415927 in
  2.0 *. r *. pi
;;

let div n m =
  (float_of_int n)/.(float_of_int m)
;;

(* Expressions de type string *)
let rec bis s = s^s;;

let rec times8 s =
  let rec aux c n = match n with
    | 0 -> c
    | _ -> (aux (c^c) (n-1))
  in aux s 3
;;

let times8_bis s = bis(bis(bis s));;

(* Booléens et conditionelles *)
let is_zero x = match x with
  | 0 -> true
  | _ -> false
;;

let msg_zero x = match (is_zero x) with
  | false -> "not zero"
  | _ -> "zero"
;;

let my_max a b = match (a>b) with
  | true -> a
  | _ -> b
;;

let max_triple a b c = match (my_max a b) with
  | d when d=a -> my_max a c
  | _ -> my_max b c
;;

let max_quadruple a b c d = match (max_triple a b c) with
  | e when e=a -> my_max a d
  | _ -> max_triple b c d
;;