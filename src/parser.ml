open Lexer
open Lang
open List

let is_function (t: token) : bool =
  match t with
  | TPlus -> true
  | TMinus -> true
  | TTimes -> true
  | TDivide -> true
  | TLessThanEq -> true
  | TIf -> true
  | _ -> false

let is_bin_op (t: token) : bool =
  match t with
  | TPlus -> true
  | TMinus -> true
  | TTimes -> true
  | TDivide -> true
  | TLessThanEq -> true
  | _ -> false

let is_tern_op (t: token) : bool =
  match t with
  | TIf -> true
  | _ -> false

let bin_op_of_tok (t: token) : binary_op =
  match t with
  | TPlus -> OAdd
  | TMinus -> OSubtract
  | TTimes -> OMultiply
  | TDivide -> ODivide
  | TLessThanEq -> OLessThanEq
  | _ -> failwith("Not a binary operator, given: " ^ (string_of_token t))

let tern_op_of_tok (t: token) : ternary_op =
  match t with
  | TIf -> OIf
  | _ -> failwith("Not a ternary operator, given: " ^ (string_of_token t))

let consume_tok (tlist: token list) (t: token) (*: bool, token list*) =
  if (length tlist) = 0
  then failwith ("Syntax error, missing a " ^ (string_of_token t))
  else
    let head = hd tlist in
    if token_eq head t then (true, tl tlist) else (false, tlist)

let rec parse (tlist: token list) =
  match tlist with
  | TOpenParen :: tail ->
    let fun_tok = (hd tail) in
    let rest = (tl tail) in
    if not (is_function fun_tok)
    then failwith ("Syntax error: \'(' should be followed by a function," ^
                   "given: " ^ (string_of_token fun_tok))
    else
      begin
        let tok_is_bin_op = is_bin_op fun_tok in
        let (expr1, tlist') = parse rest in
        let (expr2, tlist'') = parse tlist' in
        let (expr3, tlist''') = if tok_is_bin_op then (ELit (LInt 0), tlist'')
                                else parse tlist'' in
        let (close_consumed, tlist_closed) = consume_tok tlist''' TCloseParen in
        if close_consumed then
          begin
            if tok_is_bin_op
            then (EBinOp ((bin_op_of_tok fun_tok), expr1, expr2), tlist_closed)
            else (ETernOp ((tern_op_of_tok fun_tok), expr1, expr2, expr3)
                 , tlist_closed)
          end
        else failwith "Error, missing \')\'"
      end
  | TLitI i :: rest -> (ELit (LInt i), rest)
  | TLitB b :: rest -> (ELit (LBool b), rest)
  | t -> failwith ("Syntax error: "
         ^ "code should be formatted in the form (function expr1 expr2)")

let parse_tokens (token_list: token list) : ast =
  match token_list with
  | TOpenParen :: _ ->
    begin
      match (parse token_list) with
      | (expr, tlist) -> if (length tlist) = 0 then Root expr
        else failwith ("Error: program should compute one expr, remaining: "
                      ^ (String.concat "" (map string_of_token tlist)))
    end
  | t :: _ -> failwith ("Syntax error: File should begin with a function call"
                      ^ " (the \'(\' character, given: " ^ (string_of_token t))
  | [] -> failwith ("Syntax error: File should contain at least one token")
