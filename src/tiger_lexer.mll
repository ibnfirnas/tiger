{ open Printf

  module Token = Tiger_token


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

  (* Keywords *)
  | "array"    {Token.ARRAY}
  | "end"      {Token.END}
  | "in"       {Token.IN}
  | "int"      {Token.INT}
  | "let"      {Token.LET}
  | "of"       {Token.OF}
  | "type"     {Token.TYPE}
  | "var"      {Token.VAR}
  | "break"    {Token.BREAK}
  | "do"       {Token.DO}
  | "else"     {Token.ELSE}
  | "for"      {Token.FOR}
  | "function" {Token.FUNCTION}
  | "if"       {Token.IF}
  | "nil"      {Token.NIL}
  | "then"     {Token.THEN}
  | "to"       {Token.TO}
  | "while"    {Token.WHILE}
  | "string"   {Token.STRING}

  (* Identifiers *)
  | (['a'-'z' 'A'-'Z']['_' 'a'-'z' 'A'-'Z' '0'-'9']+ as id) {Token.ID id}
  | (['a'-'z' 'A'-'Z'] as id) {Token.ID (sprintf "%c" id)}

  (* Numbers *)
  | (['0'-'9']+ as number) {Token.INT_LITERAL (int_of_string number)}

  (* Eat whitespace *)
  | [' ' '\t' '\n' '\r']+ {tokens lexbuf}

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