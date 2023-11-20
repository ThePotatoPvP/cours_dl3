(* Morpion début *)

let plateau_initial (taille : int) : plateau =
  Matrix.init taille (fun (a,b) -> None)
;;

let coup_legal (plateau : plateau) (coup : coup) : bool =
  Matrix.get plateau coup.position == None
;;

let prepare_coup
    (plateau : plateau)
    (dernier_coup : coup option)
    (nb_joueurs : int)
    (x , y : int*int)
  : coup option =
  match x,y,dernier_coup with
  | a,_,_ when a >= Matrix.size plateau || a <0-> None
  | _,a,_ when a >= Matrix.size plateau || a <0-> None
  | _,_,None -> Some({joueur=0; position=(x,y)})
  | _,_,Some(m) -> Some{joueur=((m.joueur+1)mod nb_joueurs); position=(x,y)}
;;

(* Morpion condition de victoire *)

let ligne (m : 'a Matrix.t) (i : int) : 'a list =
  List.init (Matrix.size m) (fun n -> Matrix.get m (i,n))
;;

let colonne (m : 'a Matrix.t) (j : int) : 'a list =
  List.init (Matrix.size m) (fun n -> Matrix.get m (n,j))
;;

let diagonale (m : 'a Matrix.t) (k : int) : 'a list = match k with
  | 0 -> List.init (Matrix.size m) (fun n -> Matrix.get m (n,n))
  | _ -> List.init (Matrix.size m) (fun n -> Matrix.get m (n,(Matrix.size m)-n-1))
;;

let rec gagnant_liste (l : 'a option list) : 'a option = match l with
  | [] -> None
  | [Some(r)] -> Some(r)
  | Some(a)::Some(b)::q when a=b -> gagnant_liste (Some(b)::q)
  | _ -> None
;;

let premier_succes (n : int)(f : int -> 'a option) : 'a option =
  let rec aux p = match p with
    | m when n=m -> None
    | m when (f m)<>None -> f m
    | m -> aux (m+1)
  in aux 0
;;

let gagnant_lignes (m : 'a option Matrix.t) : 'a option =
  premier_succes (Matrix.size m) (fun i -> gagnant_liste (ligne m i))
;;

let gagnant_colonnes (m : 'a option Matrix.t) : 'a option =
  premier_succes (Matrix.size m) (fun i -> gagnant_liste (colonne m i))
;;

let gagnant_diagonales (m : 'a option Matrix.t) : 'a option =
  premier_succes 2 (fun i -> gagnant_liste (diagonale m i))
;;

let gagnant (m : 'a option Matrix.t) : 'a option = match (gagnant_lignes m, gagnant_colonnes m) with
  | None, None -> gagnant_diagonales m
  | Some(a),_ | _,Some(a) -> Some(a)
;;

let termine (m : 'a option Matrix.t) : bool = match gagnant m with
  | Some(_) -> true
  | None -> List.for_all (fun g -> g <> None) (List.flatten (List.init (Matrix.size m) (fun i -> ligne m i)))
;;

(* Meta morpion début *)

let plateau_initial (taille : int) : plateau =
  Matrix.init taille (fun (i,j) -> Matrix.init taille (fun (k,l)->None))
;;

let sous_morpion_valide
    (plateau : plateau)
    (dernier_coup : coup option)
    ((i,j) : int * int)
  : bool = match i,j,dernier_coup with
  | a,_,_ when a<0 || a >= (Matrix.size plateau) -> false
  | _,a,_ when a<0 || a >= (Matrix.size plateau) -> false
  | _,_,None -> true
  | _,_,Some(t) when termine_dim_1 (Matrix.get plateau (i,j)) -> false
  | _,_,Some(t) -> true

let coup_legal
    (plateau : plateau)
    (dernier_coup : coup option)
    (coup : coup)
  : bool = match dernier_coup with
  | None -> Matrix.get (Matrix.get plateau coup.position_dim_2) coup.position_dim_1 = None
  | Some(t) -> (Matrix.get (Matrix.get plateau coup.position_dim_2) coup.position_dim_1 = None)
               && (termine_dim_1 (Matrix.get plateau t.position_dim_1) || (coup.position_dim_2 = t.position_dim_1))
;;

let prepare_coup
    (plateau : plateau)
    (dernier_coup : coup option)
    (nb_joueurs : int)
    ((x,y) : int * int)
  : coup option = match x,y,dernier_coup,(Matrix.size plateau) with
  | a,_,_,m when a > (m - 1)*(m - 1) || a<0-> None
  | _,a,_,m when a > (m - 1)*(m - 1) || a<0-> None
  | _,_,None,m -> Some({joueur=0; position_dim_2=(x/m,y/m); position_dim_1=(x mod m,y mod m)})
  | _,_,Some(t),m -> Some({joueur=(t.joueur + 1)mod nb_joueurs;
                           position_dim_2=(x/m,y/m);
                           position_dim_1=(x mod m, y mod m)})
;;

(* Mega morpion condition de victoire *)

let matrix_map (f: 'a -> 'b) (m: 'a Matrix.t): 'b Matrix.t =
  Matrix.init (Matrix.size m) (fun (i,j)-> f (Matrix.get m (i,j)))
;;

let gagnant_dim_2 (plateau : plateau): int option =
  gagnant_dim_1 (matrix_map gagnant_dim_1 plateau)
;;

let termine_dim_2 (plateau : plateau) : bool = match (gagnant_dim_2 plateau) with
  | Some(_) -> true
  | None -> (let m = matrix_map termine_dim_1 plateau in
             List.for_all (fun g -> g <> false) (List.flatten (List.init (Matrix.size m) (fun i -> ligne m i))))
;;