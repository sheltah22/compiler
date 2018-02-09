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

type bin_op =
  | Add

type tern_op =
  | If

type lit =
  | Int of int

type expr =
  | Lit of lit
  | Binary_Op of bin_op * expr * expr
  | Ternary_Op of tern_op * expr * expr * expr

type ast =
  | Root of expr

let string_of_token (t: token) : string =
    match t with
  | OpenParen -> "("
  | CloseParen -> ")"
  | Plus -> "+"
  | Lit i -> string_of_int i

let is_function (t: token) : bool =
  match t with
  | Plus -> true
  | _ -> false

let is_bin_op (t: token) : bool =
  match t with
  | Plus -> true
  | _ -> false

let is_tern_op (t: token) : bool =
  match t with
  | _ -> false

let bin_op_of_tok (t: token) : bin_op =
  match t with
  | Plus -> Add
  | _ -> failwith("Not a binary operator, given: " ^ (string_of_token t))

let tern_op_of_tok (t: token) : tern_op =
  match t with
  | _ -> failwith("Not a ternary operator, given: " ^ (string_of_token t))

let token_eq (t1: token) (t2: token) : bool =
  match t1, t2 with
  | OpenParen, OpenParen -> true
  | CloseParen, CloseParen -> true
  | Plus, Plus -> true
  | Lit n, Lit m -> true
  | _ -> false

let int_of_lit (l: lit) : int =
  match l with
  | Int i -> i
  | _ -> failwith("Literal not of int type")

let tok_of_bin_op (b: bin_op) : token =
  match b with
  | Add -> Plus
  | _ -> failwith "Error: tok_of_bin_op not given a binary operator"

let consume(tlist: token list) (t: token) (*: bool, token list*) =
  let head = hd tlist in
  if token_eq head t then (true, tl tlist) else (false, tlist)

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
            | c -> failwith ("Lexing: unknown character received: " ^ (escaped c))
        else
            match c with
            | num when (48 <= (code num)) && ((code num) <= 57) -> lex infile token_list (acc ^ (escaped num))
            | wsp when (mem (code wsp) [9; 10; 11; 12; 13; 32]) -> lex infile ((Lit (int_of_string acc)) :: token_list) ""
            | '(' -> lex infile (OpenParen :: ((Lit (int_of_string acc)) :: token_list)) ""
            | ')' -> lex infile (CloseParen :: ((Lit (int_of_string acc)) :: token_list)) ""
            | c -> failwith ("Lexing: unexpected character received: " ^ (escaped c))
    with
        End_of_file -> (rev token_list)

let lex_file (filename: string) : token list =
  lex (open_in filename) [] ""

let rec parse (tlist: token list) =
  match tlist with
  | OpenParen :: tail ->
    let fun_tok = (hd tail) in
    let rest = (tl tail) in
    if not (is_function fun_tok) then failwith ("Syntax error: \'(' should be followed by a function, given: " ^ (string_of_token fun_tok))
    else
      let (expr_1, tlist') = parse rest in
      let (expr_2, tlist'') = parse tlist' in
      let (close_consumed, tlist''') = consume tlist'' CloseParen in
      if close_consumed then (Binary_Op ((bin_op_of_tok fun_tok), expr_1, expr_2), tlist''')
      else failwith "Error, missing \')\'"
  | Lit n :: rest -> (Lit (Int n), rest)
  | _ -> failwith "Syntax error: code should be formatted in the form (function expression1 expression2)"

let parse_tokens (token_list: token list) : ast =
  match token_list with
  | OpenParen :: _ -> begin
                      match (parse token_list) with
                      | (expr, tlist) -> if (length tlist) = 0 then Root expr
                      else failwith ("Error: program should compute one expression, remaining: " ^ (String.concat "" (map string_of_token tlist)))
                      end
  | t :: _ -> failwith ("Syntax error: File should begin with a function call (the \'(\' character, given: " ^ (string_of_token t))
  | [] -> failwith ("Syntax error: File should contain at least one token")

let rec print_expr (expression: expr) =
  match expression with
  | Lit (Int i) -> (print_string (string_of_int i))
  | Binary_Op (op, expr1, expr2) -> (print_string (string_of_token (tok_of_bin_op op))); (print_expr expr1); (print_expr expr2)
  | _ -> failwith "Finish print_expr, idiot"

let print_ast (tree: ast) =
  match tree with
  | Root (expression) -> print_expr expression

let compose f g x = f (g x)

let rec interpret_expr (expression: expr) : int =
  match expression with
  | Lit (Int i) -> i
  | Binary_Op (op, expr1, expr2) ->
              begin
              match op with
              | Add -> (interpret_expr expr1) + (interpret_expr expr2)
              | _ -> failwith ("Add another op to interpret_expr, idiot")
              end
  | _ -> failwith ("Add ternary operators to interpret_expr, idiot")

let interpret_tree (syntax_tree: ast) : int =
  match syntax_tree with
  | Root (expr_init) -> interpret_expr expr_init

let main () =
  Arg.parse (Arg.align arg_list) (compose print_endline (compose string_of_int (compose interpret_tree (compose parse_tokens lex_file)))) usage_msg

let _ = if !Sys.interactive then () else main ()
