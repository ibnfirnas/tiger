open Printf


module Location = Tiger_location
module Parser = Tiger_parser


let string_of_token =
  let str p str =
    sprintf
    "Line: %0.2d, Column From: %0.2d, Column To: %0.2d -> Token: %s"
    p.Location.line
    p.Location.column_from
    p.Location.column_to
    str
  in
  function
  | Parser.AND              p       -> str p "AND"
  | Parser.ARRAY            p       -> str p "ARRAY"
  | Parser.ASSIGN           p       -> str p "ASSIGN"
  | Parser.BREAK            p       -> str p "BREAK"
  | Parser.COLON            p       -> str p "COLON"
  | Parser.COMMA            p       -> str p "COMMA"
  | Parser.DIVIDE           p       -> str p "DIVIDE"
  | Parser.DO               p       -> str p "DO"
  | Parser.DOT              p       -> str p "DOT"
  | Parser.ELSE             p       -> str p "ELSE"
  | Parser.END              p       -> str p "END"
  | Parser.EOF              p       -> str p "EOF"
  | Parser.EQ               p       -> str p "EQ"
  | Parser.FOR              p       -> str p "FOR"
  | Parser.FUNCTION         p       -> str p "FUNCTION"
  | Parser.GE               p       -> str p "GE"
  | Parser.GT               p       -> str p "GT"
  | Parser.ID               (p, id) -> str p ("ID: " ^ id)
  | Parser.IF               p       -> str p "IF"
  | Parser.IN               p       -> str p "IN"
  | Parser.INT              p       -> str p "INT"
  | Parser.LBRACE           p       -> str p "LBRACE"
  | Parser.LBRACK           p       -> str p "LBRACK"
  | Parser.LE               p       -> str p "LE"
  | Parser.LET              p       -> str p "LET"
  | Parser.LPAREN           p       -> str p "LPAREN"
  | Parser.LT               p       -> str p "LT"
  | Parser.MINUS            p       -> str p "MINUS"
  | Parser.NEQ              p       -> str p "NEQ"
  | Parser.NIL              p       -> str p "NIL"
  | Parser.INT_LITERAL      (p, n)  -> str p ("INT_LITERAL: " ^ (string_of_int n))
  | Parser.OF               p       -> str p "OF"
  | Parser.OR               p       -> str p "OR"
  | Parser.PLUS             p       -> str p "PLUS"
  | Parser.RBRACE           p       -> str p "RBRACE"
  | Parser.RBRACK           p       -> str p "RBRACK"
  | Parser.RPAREN           p       -> str p "RPAREN"
  | Parser.SEMICOLON        p       -> str p "SEMICOLON"
  | Parser.STRING           p       -> str p "STRING"
  | Parser.STRING_LITERAL   (p, s)  -> str p (sprintf "STRING_LITERAL: \"%s\"" s)
  | Parser.THEN             p       -> str p "THEN"
  | Parser.TIMES            p       -> str p "TIMES"
  | Parser.TO               p       -> str p "TO"
  | Parser.TYPE             p       -> str p "TYPE"
  | Parser.VAR              p       -> str p "VAR"
  | Parser.WHILE            p       -> str p "WHILE"
