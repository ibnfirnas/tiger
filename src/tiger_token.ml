open Printf


type position = { line_num : int
                }

type t = AND            of position
       | ARRAY          of position
       | ASSIGN         of position
       | BREAK          of position
       | COLON          of position
       | COMMA          of position
       | DIVIDE         of position
       | DO             of position
       | DOT            of position
       | ELSE           of position
       | END            of position
       | EOF            of position
       | EQ             of position
       | FOR            of position
       | FUNCTION       of position
       | GE             of position
       | GT             of position
       | ID             of position * string
       | IF             of position
       | IN             of position
       | INT            of position
       | LBRACE         of position
       | LBRACK         of position
       | LE             of position
       | LET            of position
       | LPAREN         of position
       | LT             of position
       | MINUS          of position
       | NEQ            of position
       | NIL            of position
       | INT_LITERAL    of position * int
       | OF             of position
       | OR             of position
       | PLUS           of position
       | RBRACE         of position
       | RBRACK         of position
       | RPAREN         of position
       | SEMICOLON      of position
       | STRING         of position
       | STRING_LITERAL of position * string
       | THEN           of position
       | TIMES          of position
       | TO             of position
       | TYPE           of position
       | VAR            of position
       | WHILE          of position

let to_string t =
  let str p str = sprintf "%0.2d -> %s" p.line_num str in
  match t with
  | AND              p       -> str p "AND"
  | ARRAY            p       -> str p "ARRAY"
  | ASSIGN           p       -> str p "ASSIGN"
  | BREAK            p       -> str p "BREAK"
  | COLON            p       -> str p "COLON"
  | COMMA            p       -> str p "COMMA"
  | DIVIDE           p       -> str p "DIVIDE"
  | DO               p       -> str p "DO"
  | DOT              p       -> str p "DOT"
  | ELSE             p       -> str p "ELSE"
  | END              p       -> str p "END"
  | EOF              p       -> str p "EOF"
  | EQ               p       -> str p "EQ"
  | FOR              p       -> str p "FOR"
  | FUNCTION         p       -> str p "FUNCTION"
  | GE               p       -> str p "GE"
  | GT               p       -> str p "GT"
  | ID               (p, id) -> str p ("ID: " ^ id)
  | IF               p       -> str p "IF"
  | IN               p       -> str p "IN"
  | INT              p       -> str p "INT"
  | LBRACE           p       -> str p "LBRACE"
  | LBRACK           p       -> str p "LBRACK"
  | LE               p       -> str p "LE"
  | LET              p       -> str p "LET"
  | LPAREN           p       -> str p "LPAREN"
  | LT               p       -> str p "LT"
  | MINUS            p       -> str p "MINUS"
  | NEQ              p       -> str p "NEQ"
  | NIL              p       -> str p "NIL"
  | INT_LITERAL      (p, n)  -> str p ("INT_LITERAL: " ^ (string_of_int n))
  | OF               p       -> str p "OF"
  | OR               p       -> str p "OR"
  | PLUS             p       -> str p "PLUS"
  | RBRACE           p       -> str p "RBRACE"
  | RBRACK           p       -> str p "RBRACK"
  | RPAREN           p       -> str p "RPAREN"
  | SEMICOLON        p       -> str p "SEMICOLON"
  | STRING           p       -> str p "STRING"
  | STRING_LITERAL   (p, s)  -> str p ("STRING_LITERAL: " ^ s)
  | THEN             p       -> str p "THEN"
  | TIMES            p       -> str p "TIMES"
  | TO               p       -> str p "TO"
  | TYPE             p       -> str p "TYPE"
  | VAR              p       -> str p "VAR"
  | WHILE            p       -> str p "WHILE"
