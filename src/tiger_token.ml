type t = AND
       | ARRAY
       | ASSIGN
       | BREAK
       | COLON
       | COMMA
       | DIVIDE
       | DO
       | DOT
       | ELSE
       | END
       | EOF
       | EQ
       | FOR
       | FUNCTION
       | GE
       | GT
       | ID of string
       | IF
       | IN
       | INT
       | LBRACE
       | LBRACK
       | LE
       | LET
       | LPAREN
       | LT
       | MINUS
       | NEQ
       | NIL
       | INT_LITERAL of int
       | OF
       | OR
       | PLUS
       | RBRACE
       | RBRACK
       | RPAREN
       | SEMICOLON
       | STRING
       | STRING_LITERAL of string
       | THEN
       | TIMES
       | TO
       | TYPE
       | VAR
       | WHILE

let to_string = function
  | AND              -> "AND"
  | ARRAY            -> "ARRAY"
  | ASSIGN           -> "ASSIGN"
  | BREAK            -> "BREAK"
  | COLON            -> "COLON"
  | COMMA            -> "COMMA"
  | DIVIDE           -> "DIVIDE"
  | DO               -> "DO"
  | DOT              -> "DOT"
  | ELSE             -> "ELSE"
  | END              -> "END"
  | EOF              -> "EOF"
  | EQ               -> "EQ"
  | FOR              -> "FOR"
  | FUNCTION         -> "FUNCTION"
  | GE               -> "GE"
  | GT               -> "GT"
  | ID id            -> "ID: " ^ id
  | IF               -> "IF"
  | IN               -> "IN"
  | INT              -> "INT"
  | LBRACE           -> "LBRACE"
  | LBRACK           -> "LBRACK"
  | LE               -> "LE"
  | LET              -> "LET"
  | LPAREN           -> "LPAREN"
  | LT               -> "LT"
  | MINUS            -> "MINUS"
  | NEQ              -> "NEQ"
  | NIL              -> "NIL"
  | INT_LITERAL n    -> "INT_LITERAL: " ^ (string_of_int n)
  | OF               -> "OF"
  | OR               -> "OR"
  | PLUS             -> "PLUS"
  | RBRACE           -> "RBRACE"
  | RBRACK           -> "RBRACK"
  | RPAREN           -> "RPAREN"
  | SEMICOLON        -> "SEMICOLON"
  | STRING           -> "STRING"
  | STRING_LITERAL s -> "STRING_LITERAL: " ^ s
  | THEN             -> "THEN"
  | TIMES            -> "TIMES"
  | TO               -> "TO"
  | TYPE             -> "TYPE"
  | VAR              -> "VAR"
  | WHILE            -> "WHILE"
