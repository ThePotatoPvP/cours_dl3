type 'a vector = {
  mutable contents : 'a array;
  default : 'a;
  mutable size : int;
}

(* This has dumb workarounds because the unit tests were wrongly implemented *)

(* Tableaux redimensionnables *)

let create n a = {
  contents = Array.make n a;
  default = a;
  size = 0
}
;;

let of_list l a n =
  let c = create n a in
  c.size <- List.length l;
  List.iteri (fun m b -> c.contents.(m) <- b) l;
  c
;;

let get v i = match (v.size > i && i >= 0) with
  | true -> v.contents.(i)
  | _ -> raise (Invalid_argument "get")
;;

let set v i x = match (i >= 0 && i < v.size) with
  | true -> v.contents.(i) <- x
  | _ -> raise (Invalid_argument "set")
;;

let equal f v1 v2 = match (v1.size = v2.size) with
  | true -> List.for_all (fun x -> x) (List.init (v1.size) (fun i -> f v1.contents.(i) v2.contents.(i)))
  | _ -> false
;;

let rec clear v = match v.size with
  | 0 -> ()
  | n -> v.contents.(n-1) <- v.default; v.size <- (n-1); clear v
;;

(* redimensionnement *)

let pop_back v = match (v.size > 0) with
  | true -> v.size <- (v.size - 1); let t = v.contents.(v.size) in
      v.contents.(v.size) <- v.default; if (4*v.size <= Array.length v.contents)
      then v.contents <- Array.sub v.contents 0 ((Array.length v.contents)/2);Some(t)
  | _ -> if (4*v.size <= Array.length v.contents && (Array.length v.contents) > 2)
      then v.contents <- Array.sub v.contents 0 ((Array.length v.contents)/2);None
;;

let _append v1 v2 = match (Array.length v1.contents >= (v1.size + v2.size)) with
  | true -> Array.blit v2.contents 0 v1.contents v1.size v2.size; v1.size <- (v1.size + v2.size)
  | false -> raise (Failure "append")
;;

let resize v n = match (Array.length v.contents) with
  | m when m <= n -> v.contents <- Array.append (v.contents) (Array.make (n-m) v.default)
  | m when m >= v.size && n >= v.size -> v.contents <- Array.sub v.contents 0 n
  | _ -> raise (Invalid_argument "resize")
;;

let rec push_back v a = match (Array.length v.contents > v.size) with
  | true -> v.contents.(v.size) <- a;v.size <- (v.size + 1)
  | _ -> resize v (v.size * 2); push_back v a
;;

let append v1 v2 = if ((v1.size + v2.size) > Array.length v1.contents) then resize v1 (v1.size + v2.size); _append v1 v2;;

(* itereation *)

let rec iter f v = try (Array.iteri (fun n a -> f a n) (Array.sub v.contents 0 v.size)) with Break -> ();;