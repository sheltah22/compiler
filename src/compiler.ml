(* compiler.ml*)
open Pervasives
open Lang
open Lexer
open Parser

let arg_list = []

let usage_msg =
    "Usage: compiler [flags] [args]\nAvailable flags:"

let main () =
  Arg.parse
  (Arg.align arg_list)
  (fun x -> (lex_file x |> parse_tokens |> interpret_tree |> print_expr))
  usage_msg

let _ = if !Sys.interactive then () else main ()
