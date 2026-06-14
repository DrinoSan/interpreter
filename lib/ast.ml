type node =
  | Program of program
  | Expression of expression
  | Statement of statement
[@@deriving show { with_path = false }]

and expression =
  | Identifier of identifier
  | Integer of int
  | Boolean of bool
  | String of string
  | If of {
      condition : expression;
      consequences : block;
      alternative : block option;
    }
  | FunctionLiteral of { parameters : identifier list; body : block }
  | Call of { fn : expression; args : expression list }

and statement =
  | Let of { name : identifier; value : expression }
  | Return of expression
  | ExpressionStatement of expression
  | BlockStatement of block

and identifier = { identifier : string }
and block = { block : statement list }
and program = { statements : statement list }
