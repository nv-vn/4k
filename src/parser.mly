%{
  open Ast
%}

%token EOL
%token <float> FLOAT
%token PLUS MINUS TIMES DIVIDE
%token REDUCE

%left PLUS MINUS
%left TIMES DIVIDE
%left REDUCE

%start <Ast.ast> main

%%

main:
  | expr EOL
    { $1 }
  | float_list EOL
    { List $1 }
  | EOL
    { List [] }
operator:
  | PLUS
    { Plus }
  | MINUS
    { Minus }
  | TIMES
    { Times }
  | DIVIDE
    { Divide }
  | REDUCE
    { Reduce }
expr:
  | float_list
    { List $1 }
  | operator expr
    { Operation (Prefix ($1, $2)) }
  | expr operator expr
    { Operation (Infix ($2, $1, $3)) }
  | operator REDUCE expr
    { Operation (ReduceOp ($1, $3)) }
float_list:
  | FLOAT
    { [$1] }
  | FLOAT float_list
    { $1 :: $2 }
