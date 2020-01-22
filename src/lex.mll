{
  open Parse

  let get_pos lexbuf =
    let posS = Lexing.lexeme_start_p lexbuf in
    let posE = Lexing.lexeme_end_p lexbuf in
    let fname = posS.Lexing.pos_fname in
    let lnum = posS.Lexing.pos_lnum in
    let cnumS = posS.Lexing.pos_cnum - posS.Lexing.pos_bol in
    let cnumE = posE.Lexing.pos_cnum - posE.Lexing.pos_bol in
      Range.make fname lnum cnumS cnumE
}

let space = [' ' '\t' '\n' '\r']
let digit = ['0'-'9']
let alpha = ( ['a'-'z'] | ['A'-'Z'] )
let symbol = ( [' '-'@'] | ['['-'`'] | ['{'-'~'] )
let opsymbol = ( '+' | '-' | '*' | '/' | '^' | '&' | '|' | '!' | ':' | '=' | '<' | '>' | '~' | '\'' | '.' | '?' )
let alnum = digit | alpha
let string = [^ ' ' '\t' '\n' '\r' '@' '`' '"' '\\' '{' '}' '%' '|' '*' '$' '#' ';' ',' '[' ']' '(' ')']

rule lex = parse
  | space+   { lex lexbuf }
  | "\"" {
      let pos = get_pos lexbuf in
      DoubleQuotation(pos)
    }
  | '(' {
      let pos = get_pos lexbuf in
      LPAREN(pos)
    }
  | ')' {
      let pos = get_pos lexbuf in
      RPAREN(pos)
    }
  | '[' {
      let pos = get_pos lexbuf in
      LBRAC(pos)
    }
  | ']' {
      let pos = get_pos lexbuf in
      RBRAC(pos)
    }
  | ':' {
      let pos = get_pos lexbuf in
      Colon(pos)
    }
  | ';' {
      let pos = get_pos lexbuf in
      SemiColon(pos)
    }
  | ',' {
      let pos = get_pos lexbuf in
      Comma(pos)
    }
  | digit+ as n {
      let pos = get_pos lexbuf in
      Int(pos, int_of_string n)
    }
  | string+ as str {
      let pos = get_pos lexbuf in
      String(pos, str)
    }
  | eof {EOF}
 