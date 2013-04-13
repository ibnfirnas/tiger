type t = { line        : int
         ; column_from : int
         ; column_to   : int
         }


val of_lexbuf : Lexing.lexbuf -> t
