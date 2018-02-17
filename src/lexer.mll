{
open Lexing
open Parser

exception Lexer_error of string

let symbols : (string * Parser.token) list =
  [ ("(", LPAREN)
  ; (")", RPAREN)
  ; ("+", PLUS)
  ; ("-", MINUS)
  ; ("*", TIMES)
  ; ("/", DIVIDE)
  ; ("<=", LEQ)
  ; ("if", IF)
  ]

let create_symbol lexbuf =
  let str = lexeme lexbuf in
  List.assoc str symbols
}

let newline    = '\n' | ('\r' '\n') | '\r'
let whitespace = ['\t' ' ']
let digit      = ['0'-'9']

rule token = parse
  | eof                       { EOF }
  | digit+                    { INT (int_of_string (lexeme lexbuf)) }
  | whitespace+ | newline+    { token lexbuf }
  | '(' | ')' | '+' | '-'
  | '*' | '/'                 { create_symbol lexbuf }
  | "<=" | "if"               { create_symbol lexbuf }
  | "true" | "false"          { BOOL (bool_of_string (lexeme lexbuf)) }
  | _ as c { raise @@ Lexer_error ("Unexpected character: " ^ Char.escaped c) }
