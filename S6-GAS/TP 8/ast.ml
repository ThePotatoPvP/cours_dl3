type expression =
  | Var of string
  | True
  | False
  | Or of expression * expression
  | And of expression * expression
  | Define of expression * expression

let rec as_string = function
  | Var x -> x
  | True -> "true"
  | False -> "false"
  | Or (l, r) -> apply "\\/" l r
  | And (l, r) when l <> r -> apply "/\\" l r
  | And (l, _) -> "(" ^ as_string l ^ ")"

and apply op l r =
  "(" ^ as_string l ^ ") " ^ op ^ " (" ^ as_string r ^ ")"
