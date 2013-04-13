{ open Printf

  module Lex = Lexing
  module Map = Map.Make (String)

  module Debug = Tiger_debug
  module Location = Tiger_location
  module Parser = Tiger_parser


  let comment_level = ref 0

  let string_buff = Buffer.create 100

  let get_position lexbuf =
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
    [ "array"    , (fun lexbuf -> Parser.ARRAY    (get_position lexbuf))
    ; "break"    , (fun lexbuf -> Parser.BREAK    (get_position lexbuf))
    ; "do"       , (fun lexbuf -> Parser.DO       (get_position lexbuf))
    ; "else"     , (fun lexbuf -> Parser.ELSE     (get_position lexbuf))
    ; "end"      , (fun lexbuf -> Parser.END      (get_position lexbuf))
    ; "for"      , (fun lexbuf -> Parser.FOR      (get_position lexbuf))
    ; "function" , (fun lexbuf -> Parser.FUNCTION (get_position lexbuf))
    ; "if"       , (fun lexbuf -> Parser.IF       (get_position lexbuf))
    ; "in"       , (fun lexbuf -> Parser.IN       (get_position lexbuf))
    ; "int"      , (fun lexbuf -> Parser.INT      (get_position lexbuf))
    ; "let"      , (fun lexbuf -> Parser.LET      (get_position lexbuf))
    ; "nil"      , (fun lexbuf -> Parser.NIL      (get_position lexbuf))
    ; "of"       , (fun lexbuf -> Parser.OF       (get_position lexbuf))
    ; "string"   , (fun lexbuf -> Parser.STRING   (get_position lexbuf))
    ; "then"     , (fun lexbuf -> Parser.THEN     (get_position lexbuf))
    ; "to"       , (fun lexbuf -> Parser.TO       (get_position lexbuf))
    ; "type"     , (fun lexbuf -> Parser.TYPE     (get_position lexbuf))
    ; "var"      , (fun lexbuf -> Parser.VAR      (get_position lexbuf))
    ; "while"    , (fun lexbuf -> Parser.WHILE    (get_position lexbuf))
    ]
}


rule tokens = parse
  | eof {Parser.EOF (get_position lexbuf)}

  (* Comments *)
  | "/*"
  { incr comment_level;
    comment lexbuf
  }

  (* Punctuation *)
  | ":=" {Parser.ASSIGN    (get_position lexbuf)}
  | "<=" {Parser.LE        (get_position lexbuf)}
  | "<>" {Parser.NEQ       (get_position lexbuf)}
  | ">=" {Parser.GE        (get_position lexbuf)}
  | '&'  {Parser.AND       (get_position lexbuf)}
  | '('  {Parser.LPAREN    (get_position lexbuf)}
  | ')'  {Parser.RPAREN    (get_position lexbuf)}
  | '*'  {Parser.TIMES     (get_position lexbuf)}
  | '+'  {Parser.PLUS      (get_position lexbuf)}
  | ','  {Parser.COMMA     (get_position lexbuf)}
  | '-'  {Parser.MINUS     (get_position lexbuf)}
  | '.'  {Parser.DOT       (get_position lexbuf)}
  | '/'  {Parser.DIVIDE    (get_position lexbuf)}
  | ':'  {Parser.COLON     (get_position lexbuf)}
  | ';'  {Parser.SEMICOLON (get_position lexbuf)}
  | '<'  {Parser.LT        (get_position lexbuf)}
  | '='  {Parser.EQ        (get_position lexbuf)}
  | '>'  {Parser.GT        (get_position lexbuf)}
  | '['  {Parser.LBRACK    (get_position lexbuf)}
  | ']'  {Parser.RBRACK    (get_position lexbuf)}
  | '{'  {Parser.LBRACE    (get_position lexbuf)}
  | '|'  {Parser.OR        (get_position lexbuf)}
  | '}'  {Parser.RBRACE    (get_position lexbuf)}

  (* String literals *)
  | '"' {string_literal lexbuf}

  (* Identifiers *)
  | (['a'-'z' 'A'-'Z'](['_' 'a'-'z' 'A'-'Z' '0'-'9']+)? as id)
  { try
      (Map.find id keyword_token_funs) lexbuf
    with Not_found ->
      Parser.ID (get_position lexbuf, id)
  }

  (* Numbers *)
  | (['0'-'9']+ as number)
  {Parser.INT_LITERAL (get_position lexbuf, (int_of_string number))}

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
    Parser.STRING_LITERAL (get_position lexbuf, str)
  }

  | (_ as c)
  { Buffer.add_char string_buff c;
    string_literal lexbuf
  }

and comment = parse
  | eof {Parser.EOF (get_position lexbuf)}

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
