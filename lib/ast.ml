type node =
  | Program of program
  | Expression of expression
  | Statement of statement
[@@derive show { with_path = false }, sexp]

and expression =
  | Identifier of identifier
  | Integer of int
  | Boolean of bool
  | String of string

and statement =
  | Let of { name : identifier; value : expression }
  | Return of expression
  | ExpressionStatement of expression
  | BlockStatement of block

and identifier = { identifier : string }
and block = { block : statement list }
and program = { statements : statement list }
