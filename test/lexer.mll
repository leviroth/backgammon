{
open Lexing
open Parser

exception SyntaxError of string

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
    }

let convert_char c = int_of_char c - int_of_char '0'
}

let digit = ['0' - '9']
let digits = digit+
let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
rule read =
  parse
  | white                                   { read lexbuf }
  | newline                                 { next_line lexbuf; read lexbuf }
  | ';'                                     { comment lexbuf }
  | "Doubles => " (digits as d)             { DOUBLE (int_of_string d) }
  | "Takes"                                 { TAKE }
  | (digits as d1) '/' (digits as d2) '*'?  { PLAY (int_of_string d1, int_of_string d2) }
  | (digit as d1) (digit as d2) ':'         { DICE (convert_char d1, convert_char d2) }
  | (digits as d) ')'                       { TURN (int_of_string d) }
  | eof                                     { EOF }

and comment =
  parse
  | newline { read lexbuf }
  | eof { EOF }
  | _ { comment lexbuf }
