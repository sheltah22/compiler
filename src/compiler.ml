(* compiler.ml*)
open Pervasives
open List
open Char

let arg_list =
    [ ("-length",
    Arg.Rest (fun str -> print_endline (string_of_int (String.length str))),
    "Prints the length of all arguments");
    ]

let usage_msg =
    "Usage: compiler [flags] [args]\nAvailable flags:"

type token =
    | OpenParen
    | CloseParen
    | Plus
    | Lit of int

let print_token (t: token) : unit =
    match t with
    | OpenParen -> print_endline (escaped '(')
    | CloseParen -> print_endline (escaped ')')
    | Plus -> print_endline (escaped '+')
    | Lit (int) -> print_endline (string_of_int int);
    ()

type expr =
    | Add of expr * expr
    | Lit of int

let rec lex (infile: in_channel) (token_list: token list) (acc : string) : token list =
    try
        let c = (input_char infile) in
        if String.equal acc "" then
            match c with
            | '(' -> lex infile (OpenParen :: token_list) ""
            | ')' -> lex infile (CloseParen :: token_list) ""
            | '+' -> lex infile (Plus :: token_list) ""
            | wsp when (mem (code wsp) [9; 10; 11; 12; 13; 32]) -> lex infile token_list ""
            | num when (48 <= (code num)) && ((code num) <= 57) -> lex infile token_list (escaped num)
            | _ -> lex infile token_list "" (*Throw error here!*)
        else
            match c with
            | num when (48 <= (code num)) && ((code num) <= 57) -> lex infile token_list (acc ^ (escaped num))
            | wsp when (mem (code wsp) [9; 10; 11; 12; 13; 32]) -> lex infile ((Lit (int_of_string acc)) :: token_list) ""
            | ')' -> lex infile (CloseParen :: ((Lit (int_of_string acc)) :: token_list)) ""
            | _ -> lex infile token_list "" (*Throw error here!!!*)
    with
        End_of_file -> (rev token_list)

let lex_file (filename: string) : unit =
    let token_list = lex (open_in filename) [] "" in
    (map print_token token_list);
    ()

let main () =
    Arg.parse (Arg.align arg_list) lex_file usage_msg

let _ = if !Sys.interactive then () else main ()
