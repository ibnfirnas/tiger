open Printf

module Lex = Lexing
module Debug = Tiger_debug
module Parser = Tiger_parser


let main () =
  let filename = Sys.argv.(1) in
  let ic = open_in filename in
  let lexbuf = Lex.from_channel ic in

  let rec read () =
    match Tiger_lexer.tokens lexbuf with
    | Parser.EOF _ ->
      ()
    | token ->
      print_endline (Debug.string_of_token token);
      read ()
  in

  read ();
  close_in ic


let () = main ()
