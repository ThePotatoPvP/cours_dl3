type uop = Cos | Sin
type bop = Plus | Moins | Fois
type fonction = { param : string; corps : expr }
and expr =
  | Const of float
  | Var of string
  | Uop of uop * expr
  | Bop of bop * expr * expr
  | App of fonction * expr

let eps = 0.0001

(* Expressions reelles symboliques *)

let string_of_uop op = match op with
  | Cos -> "cos"
  | Sin -> "sin"
;;

let string_of_bop op = match op with
  | Plus -> "+"
  | Moins -> "-"
  | Fois -> "*"
;;

let rec string_of_expr e = match e with
  | Const(f) -> string_of_float f
  | Var(s) -> s
  | Uop(a, b) -> (string_of_uop a)^"("^(string_of_expr b)^")"
  | Bop(a, b, c) -> "("^(string_of_expr b)^" "^(string_of_bop a)^" "^(string_of_expr c)^")"
;;

let eval_uop op = match op with
  | Cos -> cos
  | Sin -> sin
;;

let eval_bop op = match op with
  | Plus -> (fun x y -> x +. y)
  | Moins -> (fun x y -> x -. y)
  | Fois -> (fun x y -> x *.y )
;;

let rec eval_expr env e = match e with
  | Const a -> a
  | Var a -> List.assoc a env
  | Bop(a, b, c) -> (eval_bop a) (eval_expr env b) (eval_expr env c)
  | Uop(a, b) -> (eval_uop a) (eval_expr env b)
;;

(* Fonctions reelles symobliques *)

let rec string_of_expr e = match e with
  | Const f -> string_of_float f
  | Var f -> f
  | Uop(a, b) -> (string_of_uop a)^"("^(string_of_expr b)^")"
  | Bop(a, b, c) -> "("^(string_of_expr b)^" "^(string_of_bop a)^" "^(string_of_expr c)^")"
  | App(a, b) -> "("^(string_of_fonction a)^")("^(string_of_expr b)^")"
and string_of_fonction f =
  f.param^" |-> "^(string_of_expr f.corps)
;;

let rec eval_expr env e = match e with
  | Const f -> f
  | Var f -> List.assoc f env
  | Uop(a, b) -> (eval_uop a) (eval_expr env b)
  | Bop(a, b, c) -> (eval_bop a) (eval_expr env b) (eval_expr env c)
  | App(a, b) -> eval_fonction env a (eval_expr env b)
and eval_fonction env f arg = (eval_expr ((f.param, arg)::env) f.corps);;

let simpl_plus e1 e2 = match e1,e2 with
  | Const 0.,_ -> e2
  | _,Const 0. -> e1
  | Const a,Const b -> Const (a+.b)
  | _,_ -> Bop(Plus, e1, e2)
;;

let simpl_moins e1 e2 = match e1,e2 with
  | Const a,Const b -> Const(a-.b)
  | _ -> Bop(Moins, e1, e2)
;;

let simpl_fois e1 e2 = match e1,e2 with
  | Const a,Const b -> Const(a*.b)
  | _,_ -> Bop(Fois, e1, e2)
;;

let simpl_bop op e1 e2 = match op with
  | Plus -> simpl_plus e1 e2
  | Moins -> simpl_moins e1 e2
  | Fois -> simpl_fois e1 e2
;;

let simpl_uop op e = match e with
  | Const a -> Const ((eval_uop op) a)
  | b -> Uop(op, b)
;;

let rec simpl_expr e = match e with
  | Const f -> Const f
  | Var f -> Var f
  | Uop(a, b) -> simpl_uop a (simpl_expr b)
  | Bop(a, b, c) -> simpl_bop a (simpl_expr b) (simpl_expr c)
  | App(a, b) -> App(simpl_fonction a, simpl_expr b)
and simpl_fonction f = {param=f.param; corps=(simpl_expr f.corps)};;

(* Methode de Newton *)

let rec deriv_expr x e = match e with
  | Const _ -> Const 0.
  | Var t when t=x -> Const 1.
  | Var t -> Const 0.
  | Uop(Cos, a) -> Bop(Moins, Const 0., Bop(Fois, deriv_expr x a, Uop(Sin, a)))
  | Uop(Sin, a) -> Bop(Fois, deriv_expr x a, Uop(Cos, a))
  | Bop(Plus, a, b) -> Bop(Plus, deriv_expr x a, deriv_expr x b)
  | Bop(Moins, a, b) -> Bop(Moins, deriv_expr x a, deriv_expr x b)
  | Bop(Fois, a, b) -> Bop(Plus, Bop(Fois, deriv_expr x a, b), Bop(Fois, a, deriv_expr x b))
  | App(f, e) -> Bop(Fois, App(deriv_fonction f, e), deriv_expr x e)
and deriv_fonction f = {param=f.param; corps=(deriv_expr f.param f.corps)};;

let newton_iter f' xi yi = xi -. (yi /. (eval_fonction [] f' xi)) ;;

let rec newton f x0 = match (eval_fonction [] f x0) with
  | a when (abs_float a)<eps -> x0
  | yi -> newton f (newton_iter (deriv_fonction f) x0 yi)
;;

let resout_eq f g xini =
  let rec a_rn g x = match g with
    | Var t -> Var x
    | Bop(a, b, c) -> Bop(a, a_rn b x, a_rn c x)
    | Uop(a, b) -> Uop(a, a_rn b x)
    | e -> e
  in newton {param="x"; corps=Bop(Moins, f.corps, a_rn g.corps f.param)} xini;;

let extremum_local f x0 = newton (deriv_fonction f) x0;;