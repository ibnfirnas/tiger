{ open Printf

  module Lex = Lexing
  module Map = Map.Make (String)

  module Debug = Tiger_debug
  module Location = Tiger_location
  module Parser = Tiger_parser


  let comment_level = ref 0

  let string_buff = Buffer.create 100

  let get_location lexbuf =
    let abs_line_start = lexbuf.Lex.lex_start_p.Lex.pos_bol in
    let abs_token_start = lexbuf.Lex.lex_start_p.Lex.pos_cnum in
    let abs_current = lexbuf.Lex.lex_curr_p.Lex.pos_cnum in
    let rel_token_start = abs_token_start - abs_line_start in
    let rel_current = abs_current - abs_line_start in

    { Location.line        = lexbuf.Lex.lex_curr_p.Lex.pos_lnum
    ; Location.column_from = rel_token_start
    ; Location.column_to   = rel_current
    }


  let keyword_token_funs =
    List.fold_left
    (fun ks (k, t) -> Map.add k t ks)
    Map.empty
    [ "array"    , (fun lexbuf -> Parser.ARRAY    (get_location lexbuf))
    ; "break"    , (fun lexbuf -> Parser.BREAK    (get_location lexbuf))
    ; "do"       , (fun lexbuf -> Parser.DO       (get_location lexbuf))
    ; "else"     , (fun lexbuf -> Parser.ELSE     (get_location lexbuf))
    ; "end"      , (fun lexbuf -> Parser.END      (get_location lexbuf))
    ; "for"      , (fun lexbuf -> Parser.FOR      (get_location lexbuf))
    ; "function" , (fun lexbuf -> Parser.FUNCTION (get_location lexbuf))
    ; "if"       , (fun lexbuf -> Parser.IF       (get_location lexbuf))
    ; "in"       , (fun lexbuf -> Parser.IN       (get_location lexbuf))
    ; "int"      , (fun lexbuf -> Parser.INT      (get_location lexbuf))
    ; "let"      , (fun lexbuf -> Parser.LET      (get_location lexbuf))
    ; "nil"      , (fun lexbuf -> Parser.NIL      (get_location lexbuf))
    ; "of"       , (fun lexbuf -> Parser.OF       (get_location lexbuf))
    ; "string"   , (fun lexbuf -> Parser.STRING   (get_location lexbuf))
    ; "then"     , (fun lexbuf -> Parser.THEN     (get_location lexbuf))
    ; "to"       , (fun lexbuf -> Parser.TO       (get_location lexbuf))
    ; "type"     , (fun lexbuf -> Parser.TYPE     (get_location lexbuf))
    ; "var"      , (fun lexbuf -> Parser.VAR      (get_location lexbuf))
    ; "while"    , (fun lexbuf -> Parser.WHILE    (get_location lexbuf))
    ]
}


rule tokens = parse
  | eof {Parser.EOF (get_location lexbuf)}

  (* Comments *)
  | "/*"
  { incr comment_level;
    comment lexbuf
  }

  (* Punctuation *)
  | ":=" {Parser.ASSIGN    (get_location lexbuf)}
  | "<=" {Parser.LE        (get_location lexbuf)}
  | "<>" {Parser.NEQ       (get_location lexbuf)}
  | ">=" {Parser.GE        (get_location lexbuf)}
  | '&'  {Parser.AND       (get_location lexbuf)}
  | '('  {Parser.LPAREN    (get_location lexbuf)}
  | ')'  {Parser.RPAREN    (get_location lexbuf)}
  | '*'  {Parser.TIMES     (get_location lexbuf)}
  | '+'  {Parser.PLUS      (get_location lexbuf)}
  | ','  {Parser.COMMA     (get_location lexbuf)}
  | '-'  {Parser.MINUS     (get_location lexbuf)}
  | '.'  {Parser.DOT       (get_location lexbuf)}
  | '/'  {Parser.DIVIDE    (get_location lexbuf)}
  | ':'  {Parser.COLON     (get_location lexbuf)}
  | ';'  {Parser.SEMICOLON (get_location lexbuf)}
  | '<'  {Parser.LT        (get_location lexbuf)}
  | '='  {Parser.EQ        (get_location lexbuf)}
  | '>'  {Parser.GT        (get_location lexbuf)}
  | '['  {Parser.LBRACK    (get_location lexbuf)}
  | ']'  {Parser.RBRACK    (get_location lexbuf)}
  | '{'  {Parser.LBRACE    (get_location lexbuf)}
  | '|'  {Parser.OR        (get_location lexbuf)}
  | '}'  {Parser.RBRACE    (get_location lexbuf)}

  (* String literals *)
  | '"' {string_literal lexbuf}

  (* Identifiers *)
  | (['a'-'z' 'A'-'Z'](['_' 'a'-'z' 'A'-'Z' '0'-'9']+)? as id)
  { try
      (Map.find id keyword_token_funs) lexbuf
    with Not_found ->
      Parser.ID (get_location lexbuf, id)
  }

  (* Numbers *)
  | (['0'-'9']+ as number)
  {Parser.INT_LITERAL (get_location lexbuf, (int_of_string number))}

  (* Track current line number *)
  | '\n' | '\r' | "\n\r"
  { Lex.new_line lexbuf;
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
    Parser.STRING_LITERAL (get_location lexbuf, str)
  }

  | (_ as c)
  { Buffer.add_char string_buff c;
    string_literal lexbuf
  }

and comment = parse
  | eof {Parser.EOF (get_location lexbuf)}

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
