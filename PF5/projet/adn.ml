type base = A | C | G | T | WC (* wildcard *)

type dna = base list



(*---------------------------------------------------------------------------*)
(*                               ECHAUFFEMENT                                *)
(*---------------------------------------------------------------------------*)


let string_of_base (b : base) : string =
  match b with
    A -> "A"
  | C -> "C"
  | G -> "G"
  | T -> "T"
  | WC -> "."
;;

(* explode a string into a char list *)
let explode (str : string) : char list =
  let t = ref [] in
  String.iter (fun c -> t := c::(!t)) str;
  List.rev !t
;;

(* conversions *)
let base_of_char (c : char) : base = match c with
  | 'A' -> A
  | 'C' -> C
  | 'G' -> G
  | 'T' -> T
  | _ -> WC
;;

let dna_of_string (s : string) : base list =
  List.map base_of_char (explode s)
;;

let string_of_dna (seq : dna) : string =
  String.concat "" (List.map string_of_base seq)
;;

(*---------------------------------------------------------------------------*)
(*                                   SLICES                                  *)
(*---------------------------------------------------------------------------*)
(*
   Une {\em tranche} de $l = \langle x_1,\dots x_n\rangle$ est une sous-liste
   de $l$ de la forme $\langle x_i, \dots x_{j}$, o\`u $1 \leq i \leq j\leq n$.
 *)

(* if list = pre@suf, return Some suf. otherwise, return None *)
let cut_prefix (slice : 'a list) (list : 'a list) : 'a list option =
  let rec aux p l = match p,l with
    | [], s -> Some(s)
    | t::q, t'::q' when t=t' -> aux q q'
    | _,_ -> None
  in aux slice list
;;

(*
  cut_prefix [1; 2; 3] [1; 2; 3; 4] = Some [4]
  cut_prefix [1; 2; 3; 4] [1; 2; 3; 4] = Some []
  cut_prefix [1; 2; 0] [1; 2; 3; 4] = None
 *)


(* return the prefix and the suffix of the first occurrence of a slice,
   or None if this occurrence does not exist.
*)

let first_occ (slice : 'a list) (list : 'a list) : ('a list * 'a list) option =
  let rec aux b s l = match s,l with
    | [],_ -> Some(b, l)
    | _,[] -> None
    | _, t::q -> (match cut_prefix s l with
      | None -> aux (b@[t]) s q
      | Some e -> Some(b, e))
  in aux [] slice list
;;

(*
  first_occ [1; 2] [1; 1; 1; 2; 3; 4; 1; 2] = Some ([1; 1], [3; 4; 1; 2])
  first_occ [1; 1] [1; 1; 1; 2; 3; 4; 1; 2] = Some ([], [1; 2; 3; 4; 1; 2])
  first_occ [1; 3] [1; 1; 1; 2; 3; 4; 1; 2] = None
 *)

let rec slices_between
          (start : 'a list) (stop : 'a list) (list : 'a list) : 'a list list =
  let rec aux acc l = match (first_occ start l) with
    | None -> acc
    | Some(b, a) -> (match (first_occ stop a) with
      | None -> acc
      | Some(b', q) -> aux (acc@[b']) q)
  in aux [] list
;;

(*
  slices_between [1; 1] [1; 2] [1; 1; 1; 1; 2; 1; 3; 1; 2] = [[1]; []; [2; 1; 3]]
 *)

let cut_genes (dna : dna) : (dna list) =
  slices_between [A; T; G] [T; A; A] dna
;;

(*---------------------------------------------------------------------------*)
(*                          CONSENSUS SEQUENCES                              *)
(*---------------------------------------------------------------------------*)


type 'a consensus = Full of 'a | Partial of 'a * int | No_consensus

(* return (Full a) if all elements of the list are equal to a,
   (Partial (a, n)) if a is the only element of the list with the
   greatest number of occurrences and this number is equal to n,
   No_consensus otherwise. *)

   let consensus (list : 'a list) : 'a consensus =
  let rec aux acc l = match l with
    | [] -> acc
    | t::q when (List.mem_assoc t acc) -> aux (List.map (fun (a, b) -> if a = t then (a, b + 1) else (a, b)) acc) q
    | t::q -> aux ((t, 1)::acc) q
  in let tor l = match l with
    | [] -> No_consensus
    | [(t,_)] -> Full t
    | (_,n)::(_,m)::_ when n=m -> No_consensus
    | (t,n)::_ -> Partial(t, n)
  in tor (List.sort (fun (a,b) (c,d) -> (d-b)) (aux [] list))
;;

(*
   consensus [1; 1; 1; 1] = Full 1
   consensus [1; 1; 1; 2] = Partial (1, 3)
   consensus [1; 1; 2; 2] = No_consensus
 *)

(* return the consensus sequence of a list of sequences : for each position
   in the elements of ll, compute the consensus  of the set of values at this
   position  in the sequences. the lists must be of same length. if all lists
   are empty, return the empty sequence.
 *)

let consensus_sequence (ll : 'a list list) : 'a consensus list =
  let rec transpose l = match l with
    | [] | []::_ -> []
    | m -> (List.map List.hd m)::(transpose (List.map List.tl m))
  in List.map consensus (transpose ll)
;;

(*
 consensus_sequence [[1; 1; 1; 1];
                     [1; 1; 1; 2];
                     [1; 1; 2; 2];
                     [1; 2; 2; 2]]
 = [Full 1; Partial (1, 3); No_consensus; Partial (2, 3)]

 consensus_sequence [[]; []; []] = []
 *)
