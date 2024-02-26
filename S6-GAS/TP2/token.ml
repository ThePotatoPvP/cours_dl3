
type token =
  INTG of int | OTHER | EOF


let to_string = function
    INTG i -> "INTG "^(string_of_int i)
  | OTHER -> "OTHER"
  | EOF -> "EOF"

