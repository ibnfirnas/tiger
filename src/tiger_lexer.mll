{ open Printf

  module Map = Map.Make (String)
  module Token = Tiger_token


  let keywords =
    List.fold_left
    (fun ks (k, t) -> Map.add k t ks)
    Map.empty
    [ "array"    , Token.ARRAY
    ; "break"    , Token.BREAK
    ; "do"       , Token.DO
    ; "else"     , Token.ELSE
    ; "end"      , Token.END
    ; "for"      , Token.FOR
    ; "function" , Token.FUNCTION
    ; "if"       , Token.IF
    ; "in"       , Token.IN
    ; "int"      , Token.INT
    ; "let"      , Token.LET
    ; "nil"      , Token.NIL
    ; "of"       , Token.OF
    ; "string"   , Token.STRING
    ; "then"     , Token.THEN
    ; "to"       , Token.TO
    ; "type"     , Token.TYPE
    ; "var"      , Token.VAR
    ; "while"    , Token.WHILE
    ]

  let comment_level = ref 0

  let string_buff = Buffer.create 100
}


rule tokens = parse
  | eof {Token.EOF}

  (* Punctuation *)
  | ":=" {Token.ASSIGN}
  | "<=" {Token.LE}
  | "<>" {Token.NEQ}
  | ">=" {Token.GE}
  | '&'  {Token.AND}
  | '('  {Token.LPAREN}
  | ')'  {Token.RPAREN}
  | '*'  {Token.TIMES}
  | '+'  {Token.PLUS}
  | ','  {Token.COMMA}
  | '-'  {Token.MINUS}
  | '.'  {Token.DOT}
  | '/'  {Token.DIVIDE}
  | ':'  {Token.COLON}
  | ';'  {Token.SEMICOLON}
  | '<'  {Token.LT}
  | '='  {Token.EQ}
  | '>'  {Token.GT}
  | '['  {Token.LBRACK}
  | ']'  {Token.RBRACK}
  | '{'  {Token.LBRACE}
  | '|'  {Token.OR}
  | '}'  {Token.RBRACE}

  (* Comments *)
  | "/*"
  { incr comment_level;
    comment lexbuf
  }

  (* String literals *)
  | '"' {string_literal lexbuf}

  (* Identifiers *)
  | (['a'-'z' 'A'-'Z'](['_' 'a'-'z' 'A'-'Z' '0'-'9']+)? as id)
  { try
      Map.find id keywords
    with Not_found ->
      Token.ID id
  }

  (* Numbers *)
  | (['0'-'9']+ as number) {Token.INT_LITERAL (int_of_string number)}

  (* Track current line number *)
  | '\n' | '\r' | "\n\r"
  { Lexing.new_line lexbuf;
    tokens lexbuf
  }

  (* Eat whitespace *)
  | [' ' '\t']+ {tokens lexbuf}

  (* Unexpected input *)
  (* TODO: Handle input errors. *)
  | _ {assert false}

and string_literal = parse
  | '\\' ('"' as c)
  { Buffer.add_char string_buff c;
    string_literal lexbuf
  }

  | '"'
  { let str = Buffer.contents string_buff in
    Buffer.reset string_buff;
    Token.STRING_LITERAL str
  }

  | (_ as c)
  { Buffer.add_char string_buff c;
    string_literal lexbuf
  }

and comment = parse
  | eof {Token.EOF}

  | "/*"
  { incr comment_level;
    comment lexbuf
  }

  | "*/"
  { decr comment_level;
    match !comment_level with
    | 0            -> tokens lexbuf
    | n when n > 0 -> comment lexbuf
    | _            -> assert false
  }

  | _ {comment lexbuf}
