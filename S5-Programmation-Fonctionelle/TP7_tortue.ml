type turtle = { x : float; y : float; angle : float; path : Vg.path }
type command =
  | Line of float
  | Move of float
  | Turn of float
  | Repeat of int * commands
and commands = command list

(* Tortue graphique *)

let rec pi = 3.1415926535897;;
let make_turtle x y = {x=x; y=y; angle=0.; path=(Path.moveto x y Path.empty)};;

let _cos f = cos ((pi /. 180.) *. f);;
let _sin f = sin ((pi /. 180.) *. f);;

let forward dist trace t =
  let rec aux nx ny = match trace with
    | true -> {x = nx; y = ny;
               angle = t.angle;
               path = Path.lineto nx ny t.path}
    | false -> {x = nx; y = ny;
                angle = t.angle;
                path = Path.moveto nx ny t.path}
  in aux (t.x +. ((_cos t.angle) *. dist)) (t.y +. ((_sin t.angle) *. dist))
;;

let rec run cmds t =
  let rec process cmd = match cmd with
    | Line f -> forward f true t
    | Move f -> forward f false t
    | Turn f -> {x=t.x; y=t.y; angle=t.angle +. f; path=t.path}
    | _ -> failwith "NotImplemented"
  in match cmds with
  | [] -> t
  | Repeat(n, cmd)::q -> run ((List.flatten (List.init n (fun i -> cmd)))@q) t
  | h::q -> run q (process h)
;;

let triangle size = [Repeat(3, [Line size; Turn 120.])];;

let square size = [Repeat(4, [Line size; Turn 90.])]

let polygon n size = [Repeat(n, [Line size; Turn (360. /. (float_of_int n))])]

let rec spiral size factor angle n = match n with
  | 0 -> []
  | _ -> [Line size; Turn angle]@(spiral (size *.factor) factor angle (n-1))
;;

(* Fractales *)

let rec sierpinski size n = match n with
  | 0 -> [Repeat(3, [Line size; Turn 120.])]
  | _ -> [Repeat(3, (sierpinski (size /. 2.) (n-1))@[Move size; Turn 120.])]
;;

let rec kochline size n = match n with
  | 0 -> [Line size]
  | _ -> (kochline (size /. 3.) (n-1))@[Turn (-60.)]@
         (kochline (size /. 3.) (n-1))@[Turn 120.]@
         (kochline (size /. 3.) (n-1))@[Turn (-60.)]@
         (kochline (size /. 3.) (n-1))
;;

let koch size n = [Repeat(3, (kochline size n)@[Turn 120.])];;

let rec rev_opp cmds = List.map (function | Turn f -> Turn (-1. *. f) | Repeat(i, c) -> Repeat(i, rev_opp c)| n -> n) (List.rev cmds);;

let rec dragon size n = match n with
  | 0 -> [Line size]
  | _ -> (dragon (size /. (sqrt 2.)) (n-1))@[Turn 90.]@(rev_opp (dragon (size /. (sqrt 2.)) (n-1)))
;;

let rec interleave x f l = match l with
  | [] -> [x]
  | t::q -> [x]@[t]@(interleave (x |> f) f q)
;;

let rec dragon_angles n = match n with
  | 0 -> []
  | _ -> interleave 90. (fun i -> (-1. *. i)) (dragon_angles (n-1))

let rec fpown f n = match n with
  | 0 -> 1.
  | _ -> f *. (fpown f (n-1))
;;

let dragon2 size n =
  let rec aux m l = match m,l with 
    | 0,_ -> [Line (size /. (fpown (sqrt 2.) n))]
    | _,t::q -> (aux 0 [])@[t]@(aux (m-1) q)
    | _,_ -> failwith "NotImplemented"
  in aux (List.length (dragon_angles n)) (List.map (fun i -> Turn i) (dragon_angles n))
;;