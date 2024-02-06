{
        open Token
}
let digit = ['0'-'9']
let intg= '-'? digit +
let flot= intg * '.' digit *
let other=[^'0'-'9']

rule lexeur = parse
        | intg as s {INTG(int_of_string s)}
        | digit as d {INTG(int_of_char d)}
        | other | flot {OTHER}
        | eof {EOF}
        | _  {exit 0}

