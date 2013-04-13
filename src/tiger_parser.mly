%{
%}


%token <Tiger_location.t>          AND
%token <Tiger_location.t>          ARRAY
%token <Tiger_location.t>          ASSIGN
%token <Tiger_location.t>          BREAK
%token <Tiger_location.t>          COLON
%token <Tiger_location.t>          COMMA
%token <Tiger_location.t>          DIVIDE
%token <Tiger_location.t>          DO
%token <Tiger_location.t>          DOT
%token <Tiger_location.t>          ELSE
%token <Tiger_location.t>          END
%token <Tiger_location.t>          EOF
%token <Tiger_location.t>          EQ
%token <Tiger_location.t>          FOR
%token <Tiger_location.t>          FUNCTION
%token <Tiger_location.t>          GE
%token <Tiger_location.t>          GT
%token <Tiger_location.t * string> ID
%token <Tiger_location.t>          IF
%token <Tiger_location.t>          IN
%token <Tiger_location.t>          INT
%token <Tiger_location.t>          LBRACE
%token <Tiger_location.t>          LBRACK
%token <Tiger_location.t>          LE
%token <Tiger_location.t>          LET
%token <Tiger_location.t>          LPAREN
%token <Tiger_location.t>          LT
%token <Tiger_location.t>          MINUS
%token <Tiger_location.t>          NEQ
%token <Tiger_location.t>          NIL
%token <Tiger_location.t * int>    INT_LITERAL
%token <Tiger_location.t>          OF
%token <Tiger_location.t>          OR
%token <Tiger_location.t>          PLUS
%token <Tiger_location.t>          RBRACE
%token <Tiger_location.t>          RBRACK
%token <Tiger_location.t>          RPAREN
%token <Tiger_location.t>          SEMICOLON
%token <Tiger_location.t>          STRING
%token <Tiger_location.t * string> STRING_LITERAL
%token <Tiger_location.t>          THEN
%token <Tiger_location.t>          TIMES
%token <Tiger_location.t>          TO
%token <Tiger_location.t>          TYPE
%token <Tiger_location.t>          VAR
%token <Tiger_location.t>          WHILE
%type <unit> main
%start main
%%


main: {()}
