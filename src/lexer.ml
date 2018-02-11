open Lang
open List

type token =
  | TOpenParen
  | TCloseParen
  | TPlus
  | TMinus
  | TTimes
  | TDivide
  | TLessThanEq
  | TIf
  | TLitI of int
  | TLitB of bool

let string_of_token (t: token) : string =
  match t with
  | TOpenParen -> "("
  | TCloseParen -> ")"
  | TPlus -> "+"
  | TMinus -> "-"
  | TTimes -> "*"
  | TDivide -> "/"
  | TLessThanEq -> "<="
  | TIf -> "if"
  | TLitI i -> string_of_int i
  | TLitB b -> string_of_bool b

let token_eq (t1: token) (t2: token) : bool =
  match t1, t2 with
  | TOpenParen, TOpenParen -> true
  | TCloseParen, TCloseParen -> true
  | TPlus, TPlus -> true
  | TMinus, TMinus -> true
  | TTimes, TTimes -> true
  | TDivide, TDivide -> true
  | TLessThanEq, TLessThanEq -> true
  | TIf, TIf -> true
  | TLitI n, TLitI m -> true
  | TLitB b, TLitB c -> true
  | _ -> false

let is_digit c =
  let code = Char.code c in
  (48 <= code) && (code <= 57)

let is_wsp c =
  let code = Char.code c in
  (List.mem code [9; 10; 11; 12; 13; 32])

let lex_file (filename: string) : token list =
  let infile = open_in filename in
  let rec consume remaining =
    if (String.length remaining) = 0 then true
      else
        let first = String.get remaining 0 in
        (Char.equal (input_char infile) first)
        && consume (String.sub remaining 1 ((String.length remaining) - 1))
  in
  let rec lex_num acc tlist : token list =
    match input_char infile with
    | num when is_digit num -> lex_num (acc ^ (Char.escaped num)) tlist
    | wsp when is_wsp wsp -> lex ((TLitI (int_of_string acc)) :: tlist)
    | '(' -> lex (TOpenParen :: ((TLitI (int_of_string acc)) :: tlist))
    | ')' -> lex (TCloseParen :: ((TLitI (int_of_string acc)) :: tlist))
    | c -> failwith ("Lexing: unexpected character received: "
                    ^ (Char.escaped c))
  and
  lex (tlist: token list) =
    try
      match input_char infile with
      | '(' -> lex (TOpenParen :: tlist)
      | ')' -> lex (TCloseParen :: tlist)
      | '+' -> lex (TPlus :: tlist)
      | '-' -> lex (TMinus :: tlist)
      | '*' -> lex (TTimes :: tlist)
      | '/' -> lex (TDivide :: tlist)
      | '<' -> if consume "=" then lex (TLessThanEq :: tlist)
               else failwith "Error: unrecognized token, did you mean <=?"
      | 'i' -> if consume "f" then lex (TIf :: tlist)
               else failwith "Error: unrecognized token, did you mean if?"
      | 't' -> if consume "rue" then lex ((TLitB (true)) :: tlist)
               else failwith "Error: unrecognized token, did you mean true?"
      | 'f' -> if consume "alse" then lex ((TLitB (false)) :: tlist)
               else failwith "Error: unrecognized token, did you mean false?"
      | num when is_digit num -> lex_num (Char.escaped num) tlist
      | wsp when is_wsp wsp -> lex tlist
      | c -> failwith ("Lexing: expected character received: " ^ (Char.escaped c));
    with
      End_of_file -> (rev tlist)
  in
    lex []
