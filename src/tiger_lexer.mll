{ open Printf

  module Map = Map.Make (String)

  module Debug    = Tiger_debug
  module Location = Tiger_location
  module Parser   = Tiger_parser


  let comment_level = ref 0

  let string_buff = Buffer.create 100

  let location =
    Location.of_lexbuf

  let find_keyword_token =
    let keyword_table =
      List.fold_left
      (fun ks (k, t) -> Map.add k t ks)
      Map.empty
      [ "array"    , (fun lb -> Parser.ARRAY    (location lb))
      ; "break"    , (fun lb -> Parser.BREAK    (location lb))
      ; "do"       , (fun lb -> Parser.DO       (location lb))
      ; "else"     , (fun lb -> Parser.ELSE     (location lb))
      ; "end"      , (fun lb -> Parser.END      (location lb))
      ; "for"      , (fun lb -> Parser.FOR      (location lb))
      ; "function" , (fun lb -> Parser.FUNCTION (location lb))
      ; "if"       , (fun lb -> Parser.IF       (location lb))
      ; "in"       , (fun lb -> Parser.IN       (location lb))
      ; "int"      , (fun lb -> Parser.INT      (location lb))
      ; "let"      , (fun lb -> Parser.LET      (location lb))
      ; "nil"      , (fun lb -> Parser.NIL      (location lb))
      ; "of"       , (fun lb -> Parser.OF       (location lb))
      ; "string"   , (fun lb -> Parser.STRING   (location lb))
      ; "then"     , (fun lb -> Parser.THEN     (location lb))
      ; "to"       , (fun lb -> Parser.TO       (location lb))
      ; "type"     , (fun lb -> Parser.TYPE     (location lb))
      ; "var"      , (fun lb -> Parser.VAR      (location lb))
      ; "while"    , (fun lb -> Parser.WHILE    (location lb))
      ]
    in
    fun ~id ~lexbuf ->
      try
        Some ((Map.find id keyword_table) lexbuf)
      with Not_found ->
        None
}


rule tokens = parse
  | eof {Parser.EOF (location lexbuf)}

  (* Comments *)
  | "/*"
  { incr comment_level;
    comment lexbuf
  }

  (* Punctuation *)
  | ":=" {Parser.ASSIGN    (location lexbuf)}
  | "<=" {Parser.LE        (location lexbuf)}
  | "<>" {Parser.NEQ       (location lexbuf)}
  | ">=" {Parser.GE        (location lexbuf)}
  | '&'  {Parser.AND       (location lexbuf)}
  | '('  {Parser.LPAREN    (location lexbuf)}
  | ')'  {Parser.RPAREN    (location lexbuf)}
  | '*'  {Parser.TIMES     (location lexbuf)}
  | '+'  {Parser.PLUS      (location lexbuf)}
  | ','  {Parser.COMMA     (location lexbuf)}
  | '-'  {Parser.MINUS     (location lexbuf)}
  | '.'  {Parser.DOT       (location lexbuf)}
  | '/'  {Parser.DIVIDE    (location lexbuf)}
  | ':'  {Parser.COLON     (location lexbuf)}
  | ';'  {Parser.SEMICOLON (location lexbuf)}
  | '<'  {Parser.LT        (location lexbuf)}
  | '='  {Parser.EQ        (location lexbuf)}
  | '>'  {Parser.GT        (location lexbuf)}
  | '['  {Parser.LBRACK    (location lexbuf)}
  | ']'  {Parser.RBRACK    (location lexbuf)}
  | '{'  {Parser.LBRACE    (location lexbuf)}
  | '|'  {Parser.OR        (location lexbuf)}
  | '}'  {Parser.RBRACE    (location lexbuf)}

  (* String literals *)
  | '"' {string_literal lexbuf}

  (* Identifiers *)
  | (['a'-'z' 'A'-'Z'](['_' 'a'-'z' 'A'-'Z' '0'-'9']+)? as id)
  { match find_keyword_token ~id ~lexbuf with
    | Some keyword_token -> keyword_token
    | None               -> Parser.ID (location lexbuf, id)
  }

  (* Numbers *)
  | (['0'-'9']+ as number)
  {Parser.INT_LITERAL (location lexbuf, (int_of_string number))}

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
    Parser.STRING_LITERAL (location lexbuf, str)
  }

  | (_ as c)
  { Buffer.add_char string_buff c;
    string_literal lexbuf
  }

and comment = parse
  | eof {Parser.EOF (location lexbuf)}

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
