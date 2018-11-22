(* display.ml *)
(* functions for printing *)
open Exps
open List

let string_of_op (op: bin_op) : string =
  match op with
  | OAdd -> "+"
  | OSubtract -> "-"
  | OMultiply -> "*"
  | ODivide -> "/"
  | OLessThanEq -> "<="
  | OGreaterThanEq -> ">="
  | OLessThan -> "<"
  | OGreaterThan -> ">"
  | OEquals -> "="

let rec string_of_type (t: typ) : string =
  match t with
  | TInt -> "int"
  | TBool -> "bool"
  | TFun (t1, t2) -> ((string_of_type t1) ^ " -> " ^ (string_of_type t2))
  | TUnit -> "unit"
  | TTuple [] -> "()"
  | TTuple (t :: rest) -> ("(" ^ (string_of_type t) ^ (String.concat "" (map (fun x -> " * " ^ x) (map string_of_type rest)) ^ ")"))
  | TList t -> ("[" ^ (string_of_type t) ^ "]")
  | TRef t -> ("<" ^ (string_of_type t) ^ ">")
  | TDynamic -> ("dynamic")

let string_of_bin_op (op: bin_op) =
  match op with
  | OAdd -> "+"
  | OSubtract -> "-"
  | OMultiply -> "*"
  | ODivide -> "/"
  | OLessThan -> "<"
  | OLessThanEq -> "<="
  | OGreaterThan -> ">"
  | OGreaterThanEq -> ">="
  | OEquals -> "="

let rec string_of_exp_parsed (e: exp) : string =
  match e with
  | EVal v -> string_of_value_parsed v
  | EBinOp (op, exp1, exp2) ->
    "(" ^ (string_of_bin_op op) ^ " " ^ (string_of_exp_parsed exp1) ^ " "
      ^ (string_of_exp_parsed exp2) ^ ")"
  | EIf (exp1, exp2, exp3) ->
    "(if " ^ (string_of_exp_parsed exp1) ^ " " ^ (string_of_exp_parsed exp2) ^ " "
      ^ (string_of_exp_parsed exp3) ^ ")"
  | EVar s -> s
  | ELet (expr1, expr2, expr3, t) -> ("(let " ^ (string_of_exp_parsed expr1)
      ^ " : " ^ (string_of_type t) ^ " = "
      ^ (string_of_exp_parsed expr2) ^ " in " ^ (string_of_exp_parsed expr3) ^ ")")
  | EFunCall (expr1, expr2) -> ("(" ^ (string_of_exp_parsed expr1) ^ " "
    ^ (string_of_exp_parsed expr2) ^ ")")
  | EHead expr -> ("(head " ^ (string_of_exp_parsed expr) ^ ")")
  | ETail expr -> ("(tail " ^ (string_of_exp_parsed expr) ^ ")")
  | EEmpty expr -> ("(empty? " ^ (string_of_exp_parsed expr) ^ ")")
  | ERef expr -> ("(ref " ^ (string_of_exp_parsed expr) ^ ")")
  | EAssign (expr1, expr2) -> ("(:= " ^ (string_of_exp_parsed expr1) ^ " " ^ (string_of_exp_parsed expr2) ^ ")")
  | EBang expr -> ("(! " ^ (string_of_exp_parsed expr) ^ ")")
  | ESequence (expr1, expr2) -> ("(; " ^ (string_of_exp_parsed expr1) ^ " " ^ (string_of_exp_parsed expr2) ^ ")")
  | EWhile (expr1, expr2, expr3) -> ("(while " ^ (string_of_exp_parsed expr1) ^ " " ^ (string_of_exp_parsed expr3) ^ ")")
  | ENth (expr1, expr2) -> ("(nth " ^ (string_of_exp_parsed expr1) ^ " " ^ (string_of_exp_parsed expr2) ^ ")")
  | EInferLet (e1, e2, e3) -> ("(let " ^ (string_of_exp_parsed e1)
    ^ " = " ^ (string_of_exp_parsed e2) ^ " in " ^ (string_of_exp_parsed e3) ^ ")")
and string_of_value_parsed (v: value) : string =
  match v with
  | VLit (LInt i) -> string_of_int i
  | VLit (LBool b) -> string_of_bool b
  | VFun (e1, e2, t1, t2) -> ("(fun (" ^ (string_of_exp_parsed e1) ^ ": "
    ^ (string_of_type t1) ^ ") : " ^ (string_of_type t2) ^ " -> "
    ^ (string_of_exp_parsed e2) ^ ")")
  | VFix (e1, e2, e3, t1, t2) -> ("(fix " ^ (string_of_exp_parsed e1) ^ " ("
    ^ (string_of_exp_parsed e2) ^ ": " ^ (string_of_type t1) ^ ") : "
    ^ (string_of_type t2) ^ " -> " ^ (string_of_exp_parsed e3) ^ ")")
  | VUnit -> "()"
  | VTuple ([]) -> "()T"
  | VTuple (e :: rest) -> ("(" ^ (string_of_exp_parsed e) ^ (String.concat "" (map (fun x -> ", " ^ x) (map string_of_exp_parsed rest))) ^ ")")
  | VEmptyList t -> ("([] : " ^ (string_of_type t) ^ ")")
  | VCons (e1, e2) -> ("(" ^ (string_of_exp_parsed e1) ^ " :: " ^ (string_of_exp_parsed e2) ^ ")")
  | VPtr i -> ("(ptr, address: " ^ (string_of_int i) ^ ")")

let rec string_of_exp (e: exp) : string =
  match e with
  | EVal v -> string_of_value v
  | EBinOp (op, e1, e2) ->
    ((string_of_exp e1) ^ " " ^ (string_of_bin_op op) ^ " " ^ (string_of_exp e2))
  | EIf (e1, e2, e3) ->
    ("if " ^ (string_of_exp e1) ^ " then " ^ (string_of_exp e2) ^ "\n    else "
    ^ (string_of_exp e3))
  | EVar s -> s
  | ELet (e1, e2, e3, t) ->
    ("let " ^ (string_of_exp e1) ^ " : " ^ (string_of_type t) ^ " = "
    ^ (string_of_exp e2) ^ " in\n    " ^ (string_of_exp e3))
  | EFunCall (e1, e2) -> ((string_of_exp e1) ^ " " ^ (string_of_exp e2))
  | EHead expr -> ("head " ^ (string_of_exp expr))
  | ETail expr -> ("tail " ^ (string_of_exp expr))
  | EEmpty expr -> ("empty? " ^ (string_of_exp expr))
  | ERef expr -> ("ref " ^ (string_of_exp expr))
  | EAssign (e1, e2) -> ((string_of_exp e1) ^ " := " ^ (string_of_exp e2))
  | EBang e -> ("! " ^ (string_of_exp e))
  | ESequence (e1, e2) -> ((string_of_exp e1) ^ "; " ^ (string_of_exp e2))
  | EWhile (e1, e2, e3) -> ("while " ^ (string_of_exp e1) ^ " do " ^ (string_of_exp e3) ^ " end")
  | ENth (e1, e2) -> ("nth " ^ (string_of_exp e1) ^ " " ^ (string_of_exp e2) ^ ")")
  | EInferLet (e1, e2, e3) -> ("let " ^ (string_of_exp_parsed e1)
    ^ " = " ^ (string_of_exp_parsed e2) ^ " in\n    " ^ (string_of_exp_parsed e3))
and string_of_value (v: value) : string =
  match v with
  | VLit (LInt i) -> string_of_int i
  | VLit (LBool b) -> string_of_bool b
  | VFun (e1, e2, t1, t2) -> ("fun (" ^ (string_of_exp e1) ^ ": "
    ^ (string_of_type t1) ^ ") : " ^ (string_of_type t2) ^ " -> " ^ (string_of_exp e2))
  | VFix (e1, e2, e3, t1, t2) -> ("fix " ^ (string_of_exp e1) ^ " (" ^ (string_of_exp e2) ^ ": "
    ^ (string_of_type t2) ^ ") : " ^ (string_of_type t2) ^ " -> "
    ^ (string_of_exp e3))
  | VUnit -> "()"
  | VTuple ([]) -> "()T"
  | VTuple (e :: rest) -> ("(" ^ (string_of_exp_parsed e) ^ (String.concat "" (map (fun x -> ", " ^ x) (map string_of_exp_parsed rest))) ^ ")")
  | VEmptyList t -> ("[] : " ^ (string_of_type t))
  | VCons (e1, e2) -> ((string_of_exp e1) ^ " :: " ^ (string_of_exp e2))
  | VPtr i -> ("(ptr, address: " ^ (string_of_int i) ^ ")")
