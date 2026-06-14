open Code

let input = {|
let fibonacci = fn(x) {
  if (x == 0) {
    0
  } else {
    if (x == 1) {
      return 1;
    } else {
      fibonacci(x - 1) + fibonacci(x - 2);
    }
  }
};
|}

let () =
  let lexer = Lexer.init input in
  let rec loop lexer =
    match Lexer.next_token lexer with
    | _, None -> ()
    | lexer, Some token ->
      print_endline (Token.show token);
      loop lexer
  in
  loop lexer