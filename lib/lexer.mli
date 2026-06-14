(* Lexer *)
type t

(* Constructor *)
val init : string -> t
val next_token : t -> t * Token.t option

(* Debug *)
val pp : Format.formatter -> t -> unit
val show : t -> string
