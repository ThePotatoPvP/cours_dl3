(* Somme *)

let rec sum n = match n with
  | k when k>0 -> k+(sum (k-1))
  | _ -> 0
;;

(* FIbonacci *)

let rec cringe_fibo n = match n with
  | 0 | 1 -> 1
  | _ -> (cringe_fibo (n-1)) + (cringe_fibo (n-2))
;;

let fibo n =
  let a = Array.make (n+1) 0 in
  let rec aux n = match n with
    | 0 | 1 -> a.(0)<-1; a.(1)<-1;
    | _ when a.(n-1)>0 -> a.(n)<-(a.(n-1)+a.(n-2))
    | _ -> aux (n-1); aux n
  in aux n;
  a.(n);
;;

(* Employés entrepôt *)

let rec pasA n = match n with
  | 0 -> 0
  | _ -> 1+ (pasB (n-1))
and pasB n = match (n mod 2) with
  | 0 when n=0 -> 0
  | 0 -> 1+(pasA (n-2))
  | 1 -> 1+(pasA (n-1))
  | _ -> 0
;;

(* Ackermann *)

let rec ack m n = match m,n with
  | 0,_ -> n+1
  | _,0 -> ack (m-1) 1
  | _,_ -> ack (m-1) (ack m (n-1))
;;