open Printf

module Token = Tiger_token


let main () =
  let filename = Sys.argv.(1) in
  let ic = open_in filename in
  let lexbuf = Lexing.from_channel ic in

  let rec read () =
    match Tiger_lexer.tokens lexbuf with
    | Token.EOF ->
      ()
    | token ->
      print_endline (Token.to_string token);
      read ()
  in

  read ();
  close_in ic


let () = main ()
