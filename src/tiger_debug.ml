open Printf


module Location = Tiger_location
module Parser   = Tiger_parser


let string_of_token =
  let str l str =
    sprintf
    "Line: %0.2d, Column From: %0.2d, Column To: %0.2d -> Token: %s"
    l.Location.line
    l.Location.column_from
    l.Location.column_to
    str
  in
  function
  | Parser.AND              l       -> str l "AND"
  | Parser.ARRAY            l       -> str l "ARRAY"
  | Parser.ASSIGN           l       -> str l "ASSIGN"
  | Parser.BREAK            l       -> str l "BREAK"
  | Parser.COLON            l       -> str l "COLON"
  | Parser.COMMA            l       -> str l "COMMA"
  | Parser.DIVIDE           l       -> str l "DIVIDE"
  | Parser.DO               l       -> str l "DO"
  | Parser.DOT              l       -> str l "DOT"
  | Parser.ELSE             l       -> str l "ELSE"
  | Parser.END              l       -> str l "END"
  | Parser.EOF              l       -> str l "EOF"
  | Parser.EQ               l       -> str l "EQ"
  | Parser.FOR              l       -> str l "FOR"
  | Parser.FUNCTION         l       -> str l "FUNCTION"
  | Parser.GE               l       -> str l "GE"
  | Parser.GT               l       -> str l "GT"
  | Parser.ID               (l, id) -> str l ("ID: " ^ id)
  | Parser.IF               l       -> str l "IF"
  | Parser.IN               l       -> str l "IN"
  | Parser.INT              l       -> str l "INT"
  | Parser.LBRACE           l       -> str l "LBRACE"
  | Parser.LBRACK           l       -> str l "LBRACK"
  | Parser.LE               l       -> str l "LE"
  | Parser.LET              l       -> str l "LET"
  | Parser.LPAREN           l       -> str l "LPAREN"
  | Parser.LT               l       -> str l "LT"
  | Parser.MINUS            l       -> str l "MINUS"
  | Parser.NEQ              l       -> str l "NEQ"
  | Parser.NIL              l       -> str l "NIL"
  | Parser.INT_LITERAL      (l, n)  -> str l ("INT_LITERAL: " ^ (string_of_int n))
  | Parser.OF               l       -> str l "OF"
  | Parser.OR               l       -> str l "OR"
  | Parser.PLUS             l       -> str l "PLUS"
  | Parser.RBRACE           l       -> str l "RBRACE"
  | Parser.RBRACK           l       -> str l "RBRACK"
  | Parser.RPAREN           l       -> str l "RPAREN"
  | Parser.SEMICOLON        l       -> str l "SEMICOLON"
  | Parser.STRING           l       -> str l "STRING"
  | Parser.STRING_LITERAL   (l, s)  -> str l (sprintf "STRING_LITERAL: \"%s\"" s)
  | Parser.THEN             l       -> str l "THEN"
  | Parser.TIMES            l       -> str l "TIMES"
  | Parser.TO               l       -> str l "TO"
  | Parser.TYPE             l       -> str l "TYPE"
  | Parser.VAR              l       -> str l "VAR"
  | Parser.WHILE            l       -> str l "WHILE"
