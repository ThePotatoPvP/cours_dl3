let prod = ref 1 in
let ch = open_in (Sys.argv.(1)) in
  let lexbuf = Lexing.from_channel ch in
  let rec f (t:Token.token ) =
    match t with
    | INTG i -> (prod := i * (!prod)) ; f (Lexeur2.lexeur lexbuf)
    | OTHER -> f (Lexeur2.lexeur lexbuf)
    | EOF -> print_int !prod; print_newline()
  in
f (Lexeur2.lexeur lexbuf);;
