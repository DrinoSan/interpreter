open Base

type t = { input : string; position : int; ch : char option } [@@deriving show]

let init input =
  if String.is_empty input then { input; position = 0; ch = None }
  else { input; position = 0; ch = Some (String.get input 0) }

let rec next_token lexer =
  let lexer = skip_whitespace lexer in
  let open Token in
  match lexer.ch with
  | None -> (lexer, None)
  | Some ch ->
      let lexer, token =
        match ch with
        | ';' -> (advance lexer, SEMICOLON)
        | '(' -> (advance lexer, LPAREN)
        | ')' -> (advance lexer, RPAREN)
        | '+' -> (advance lexer, PLUS)
        | '-' -> (advance lexer, MINUS)
        | '*' -> (advance lexer, ASTERISK)
        | '/' -> (advance lexer, SLASH)
        | '<' -> (advance lexer, LT)
        | '>' -> (advance lexer, GT)
        | '{' -> (advance lexer, LBRACE)
        | '}' -> (advance lexer, RBRACE)
        | ',' -> (advance lexer, COMMA)
        | '!' -> if_peeked lexer '=' ~default:BANG ~matched:NOT_EQ
        | '=' -> if_peeked lexer '=' ~default:ASSIGN ~matched:EQ
        | ch when is_identifier ch -> read_identifier lexer
        | ch when is_number ch -> read_number lexer
        | ch -> Fmt.failwith "Illegal character: %c" ch
      in
      (lexer, Some token)

and skip_whitespace lexer =
  let lexer, _ =
    seek lexer (fun ch ->
        match ch with Some ch -> Char.is_whitespace ch | None -> false)
  in
  lexer

and if_peeked lexer ch ~default ~matched =
  let lexer, result =
    match peek_char lexer with
    | Some peeked when Char.(peeked = ch) -> (advance lexer, matched)
    | _ -> (lexer, default)
  in
  (advance lexer, result)

and is_identifier ch = Char.(ch = '_' || is_alpha ch)

and read_while lexer condition =
  let pos_start = lexer.position in
  let lexer, pos_end =
    seek lexer (fun ch ->
        match ch with Some character -> condition character | None -> false)
  in
  (lexer, String.sub lexer.input ~pos:pos_start ~len:(pos_end - pos_start))

and read_identifier lexer =
  let lexer, ident = read_while lexer is_identifier in
  (lexer, Token.lookup_ident ident)

and is_number ch = Char.is_digit ch

and read_number lexer =
  let lexer, int = read_while lexer is_number in
  (lexer, Token.INT int)

and peek_char lexer =
  if lexer.position >= String.length lexer.input - 1 then None
  else Some (String.get lexer.input (lexer.position + 1))

and seek lexer condition =
  let rec loop lexer =
    (* This could also maybe look nice with lexer |> advance |> loop *)
    if condition lexer.ch then loop (advance lexer) else lexer 
  in
  let lexer = loop lexer in
  (lexer, lexer.position)

and advance lexer =
  let position = lexer.position + 1 in
  let ch =
    if position >= String.length lexer.input then None
    else Some (String.get lexer.input position)
  in
  { lexer with position; ch }
