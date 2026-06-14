open Base

let ( let* ) res f = Base.Result.bind res ~f

type precedence =
  [ `Lowest
  | `Equals        (* == *)
  | `LessGreater   (* < > *)
  | `Sum           (* + - *)
  | `Product       (* * / *)
  | `Prefix        (* -x !x *)
  | `Call          (* fn(x) *)
  ]

type t = { lexer : Lexer.t; current : Token.t option; peek : Token.t option }
[@@deriving show]

let advance parser =
  let lexer, token = Lexer.next_token parser.lexer in
  { lexer; current = parser.peek; peek = token }

let init lexer =
  let parser = { lexer; current = None; peek = None } in
  let parser = advance parser in
  let parser = advance parser in
  parser

let rec parse parser =
  let rec parse' parser statements =
    match parser.current with
    | Some _ -> (
        match parse_statement parser with
        | Ok (parser, stmt) -> parse' (advance parser) (stmt :: statements)
        | Error msg -> err parser msg statements)
    | None -> Ok (parser, List.rev statements)
  in
  let* _, statements = parse' parser [] in
  Ok (Ast.Program { statements })

(* I want to parse 
let x = 10; *)

and parse_statement parser =
  match parser.current with
  | Some Token.LET -> parse_let parser
  | _ -> parse_let parser

and parse_let parser =
  let* parser, name = parse_identifier parser in
  let* parser = expect_assign parser in
  let parser = advance parser in
  let* parser, value = parse_expression parser `Lowest in
  let parser = chomp_semicolon parser in
  Ok (parser, Ast.Let { name; value })

and err parser msg statments = Error msg

and parse_identifier parser =
  match parser.current with
  | Some (Token.IDENT name) -> Ok (advance parser, Ast.{ identifier = name })
  | Some token ->
      Error (Fmt.str "expected Identifier, got %s" (Token.show token))
  | None -> Error "Expected identifier, got EOF"

and expect_assign parser =
  match parser.current with 
  | Some Token.ASSIGN -> Ok( parser |> advance )
  | Some token -> Error (Fmt.str "expected '=', got %s" (Token.show token) )
  | _ -> Error "expected '=', got EOF"


