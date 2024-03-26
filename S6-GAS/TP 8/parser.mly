%{
open Ast
%}

%token OR AND LPAREN RPAREN TRUE FALSE EOF
%token<string> ID
%right Or
%right And

%start<Ast.expression> input

%%


input: c=expression EOF { c }

expression:
x=ID  { Var x }
| l=expression OR r=expression { Or (l, r) }
| l=expression AND r=expression { And (l, r) }
| TRUE { True}
| FALSE { False }
| LPAREN c=expression RPAREN { c }

