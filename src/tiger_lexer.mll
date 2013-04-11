{ open Printf

  module Map = Map.Make (String)
  module Token = Tiger_token


  let comment_level = ref 0

  let string_buff = Buffer.create 100

  let get_position lexbuf =
    { Token.line_num = lexbuf.Lexing.lex_curr_p.Lexing.pos_lnum
    }

  let keywords =
    List.fold_left
    (fun ks (k, t) -> Map.add k t ks)
    Map.empty
    [ "array"    , (fun lexbuf -> Token.ARRAY    (get_position lexbuf))
    ; "break"    , (fun lexbuf -> Token.BREAK    (get_position lexbuf))
    ; "do"       , (fun lexbuf -> Token.DO       (get_position lexbuf))
    ; "else"     , (fun lexbuf -> Token.ELSE     (get_position lexbuf))
    ; "end"      , (fun lexbuf -> Token.END      (get_position lexbuf))
    ; "for"      , (fun lexbuf -> Token.FOR      (get_position lexbuf))
    ; "function" , (fun lexbuf -> Token.FUNCTION (get_position lexbuf))
    ; "if"       , (fun lexbuf -> Token.IF       (get_position lexbuf))
    ; "in"       , (fun lexbuf -> Token.IN       (get_position lexbuf))
    ; "int"      , (fun lexbuf -> Token.INT      (get_position lexbuf))
    ; "let"      , (fun lexbuf -> Token.LET      (get_position lexbuf))
    ; "nil"      , (fun lexbuf -> Token.NIL      (get_position lexbuf))
    ; "of"       , (fun lexbuf -> Token.OF       (get_position lexbuf))
    ; "string"   , (fun lexbuf -> Token.STRING   (get_position lexbuf))
    ; "then"     , (fun lexbuf -> Token.THEN     (get_position lexbuf))
    ; "to"       , (fun lexbuf -> Token.TO       (get_position lexbuf))
    ; "type"     , (fun lexbuf -> Token.TYPE     (get_position lexbuf))
    ; "var"      , (fun lexbuf -> Token.VAR      (get_position lexbuf))
    ; "while"    , (fun lexbuf -> Token.WHILE    (get_position lexbuf))
    ]
}


rule tokens = parse
  | eof {Token.EOF (get_position lexbuf)}

  (* Punctuation *)
  | ":=" {Token.ASSIGN    (get_position lexbuf)}
  | "<=" {Token.LE        (get_position lexbuf)}
  | "<>" {Token.NEQ       (get_position lexbuf)}
  | ">=" {Token.GE        (get_position lexbuf)}
  | '&'  {Token.AND       (get_position lexbuf)}
  | '('  {Token.LPAREN    (get_position lexbuf)}
  | ')'  {Token.RPAREN    (get_position lexbuf)}
  | '*'  {Token.TIMES     (get_position lexbuf)}
  | '+'  {Token.PLUS      (get_position lexbuf)}
  | ','  {Token.COMMA     (get_position lexbuf)}
  | '-'  {Token.MINUS     (get_position lexbuf)}
  | '.'  {Token.DOT       (get_position lexbuf)}
  | '/'  {Token.DIVIDE    (get_position lexbuf)}
  | ':'  {Token.COLON     (get_position lexbuf)}
  | ';'  {Token.SEMICOLON (get_position lexbuf)}
  | '<'  {Token.LT        (get_position lexbuf)}
  | '='  {Token.EQ        (get_position lexbuf)}
  | '>'  {Token.GT        (get_position lexbuf)}
  | '['  {Token.LBRACK    (get_position lexbuf)}
  | ']'  {Token.RBRACK    (get_position lexbuf)}
  | '{'  {Token.LBRACE    (get_position lexbuf)}
  | '|'  {Token.OR        (get_position lexbuf)}
  | '}'  {Token.RBRACE    (get_position lexbuf)}

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
      (Map.find id keywords) lexbuf
    with Not_found ->
      Token.ID (get_position lexbuf, id)
  }

  (* Numbers *)
  | (['0'-'9']+ as number)
  {Token.INT_LITERAL (get_position lexbuf, (int_of_string number))}

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
    Token.STRING_LITERAL (get_position lexbuf, str)
  }

  | (_ as c)
  { Buffer.add_char string_buff c;
    string_literal lexbuf
  }

and comment = parse
  | eof {Token.EOF (get_position lexbuf)}

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
