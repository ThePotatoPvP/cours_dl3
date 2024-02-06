{
  exception Eof
  let sum = ref 0
}

let digit = ['0'-'9']

rule lexeur = parse
  | digit as c	{ sum := (int_of_char c) + (!sum) }
  | _           { lexeur lexbuf }
  | eof		{ raise Eof }

{
  let ch = open_in (Sys.argv.(1)) in
  let lexbuf = Lexing.from_channel ch in
  try
    while true do
      lexeur lexbuf
    done
  with Eof -> (print_int !sum; print_newline())
}
