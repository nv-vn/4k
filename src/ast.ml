type ast =
  | List of float list
  | Operation of application
and operator =
  | Plus
  | Minus
  | Times
  | Divide
  | Reduce
  | Stretch
and application =
  | Prefix of operator * ast
  | Infix of operator * ast * ast 
  | ReduceOp of operator * ast
  | ExpandOp of operator * ast
