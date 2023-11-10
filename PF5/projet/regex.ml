open Regex_base

let rec repeat n l =
  let rec aux acc n = match n with
    | 0 -> acc
    | m when m>0 -> aux (l@acc) (m-1)
    | _ -> failwith "NotImplemented"
  in aux [] n
;;

let rec expr_repeat n e =
  let rec aux acc n = match n with
    | 0 -> acc
    | m -> aux (Concat(acc, e)) (m-1)
  in aux Eps n
;;

let rec is_empty e = match e with
  | Eps -> true
  | Concat(a, b)| Alt(a, b) -> (is_empty a) && (is_empty b)
  | Star(a) -> is_empty a
  | Base(_) | Joker -> false
;;

let rec null e = match e with
  | Eps | Star(_) -> true
  | Concat(a, b) -> (null a) && (null b)
  | Alt(a, b) -> (null a) || (null b)
  | Base(_) | Joker -> false
;;

let rec is_finite e = match e with
  | Star r -> is_empty r
  | Concat(a, b) | Alt(a, b) -> (is_finite a) && (is_finite b)
  | Eps | Base(_) | Joker -> true
;;

let product l1 l2 =
  let rec aux acc l = match l with
    | [] -> acc
    | t::q -> aux ((List.map (fun g -> t@g) l2)@acc) q
  in aux [] l1
;;

let enumerate alphabet e =
  let rec aux e = match e with
    | Eps -> [[]]
    | Base(a) -> [[a]]
    | Joker -> List.map (fun a -> [a]) alphabet
    | Concat(a, b) -> product (aux a) (aux b)
    | Alt(a, b) -> union_sorted (aux a) (aux b)
    | Star(_) -> failwith "NotImplemented"
  in let aux2 e = match is_finite e with
    | true -> Some(aux e)
    | false -> None
  in aux2 e
;;

let rec alphabet_expr e = match e with
  | Eps | Joker -> []
  | Base(a) -> [a]
  | Star(a) -> alphabet_expr a
  | Concat(a, b) | Alt(a, b) -> union_sorted (alphabet_expr a) (alphabet_expr b)
;;

type answer =
  Infinite | Accept | Reject

let accept_partial e (w : char list) = match (enumerate (alphabet_expr e) e) with
  | None -> Infinite
  | Some(l)  when List.mem w l -> Accept
  | _ -> Reject
;;
