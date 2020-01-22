%{
  open Range
  open TmType
%}


%token <Range.t>LPAREN
%token <Range.t>RPAREN
%token <Range.t>LBRAC
%token <Range.t>RBRAC
%token <Range.t>Colon
%token <Range.t>SemiColon
%token <Range.t>Comma
%token <Range.t>DoubleQuotation

%token <Range.t * int>Int
%token <Range.t * string>String

%token EOF

%start parse
%type <TmType.t> parse

%%

parse :
  | term EOF { $1 }
;
term_tuple_cont:
  | { [] }
  | term { [$1] }
  | term Comma term_tuple_cont { $1 :: $3 }
;
listcont:
  | { [] }
  | term { [$1] }
  | term SemiColon listcont { $1 :: $3 }
;
term :
  | LPAREN term_tuple_cont RPAREN { TmTuple($2) }
  | LBRAC listcont RBRAC { TmList($2) }
  | Int {TmInt($1)}
  | DoubleQuotation String DoubleQuotation {TmString($2)}
  | LPAREN term RPAREN {$2}
;
%%
