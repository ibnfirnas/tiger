module Lex = Lexing


type t = { line        : int
         ; column_from : int
         ; column_to   : int
         }


let of_lexbuf lb =
  let abs_line_start  = lb.Lex.lex_start_p.Lex.pos_bol in
  let abs_token_start = lb.Lex.lex_start_p.Lex.pos_cnum in
  let abs_current     = lb.Lex.lex_curr_p.Lex.pos_cnum in

  let rel_token_start = abs_token_start - abs_line_start in
  let rel_current     = abs_current - abs_line_start in

  { line        = lb.Lex.lex_curr_p.Lex.pos_lnum
  ; column_from = rel_token_start
  ; column_to   = rel_current
  }
