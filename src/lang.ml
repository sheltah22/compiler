type lit =
  | LInt of int
  | LBool of bool

type bin_op =
  | OAdd
  | OSubtract
  | OMultiply
  | ODivide
  | OLessThanEq
  | OGreaterThanEq
  | OLessThan
  | OGreaterThan
  | OEquals

type value =
  | VLit of lit
  | VFun of exp * exp
  | VFix of exp * exp * exp
and exp =
  | EVal of value
  | EBinOp of bin_op * exp * exp
  | EIf of exp * exp * exp
  | EVar of string
  | ELet of exp * exp * exp
  | EFunCall of exp * exp

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

(* Copied from Matthias Braun at
 * https://stackoverflow.com/questions/9863036/ocaml-function-parameter-pattern-matching-for-strings *)
let explode str =
  let rec explode_inner cur_index chars =
    if (cur_index < String.length str) then
      let new_char = str.[cur_index] in
      explode_inner (cur_index + 1) (chars @ [new_char])
    else chars in
      explode_inner 0 []

let rec implode chars =
  match chars with
  | [] -> ""
  | h :: t -> (Char.escaped h) ^ (implode t)
(* End copied portion *)

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

let rec string_of_exp (e: exp) : string =
  match e with
  | EVal v -> string_of_value v
  | EBinOp (op, exp1, exp2) ->
    "(" ^ (string_of_bin_op op) ^ " " ^ (string_of_exp exp1) ^ " "
      ^ (string_of_exp exp2) ^ ")"
  | EIf (exp1, exp2, exp3) ->
    "(if " ^ (string_of_exp exp1) ^ " " ^ (string_of_exp exp2) ^ " "
      ^ (string_of_exp exp3) ^ ")"
  | EVar s -> s
  | ELet (expr1, expr2, expr3) -> ("(let " ^ (string_of_exp expr1) ^ " = "
      ^ (string_of_exp expr2) ^ " in " ^ (string_of_exp expr3) ^ ")")
  | EFunCall (expr1, expr2) -> ("(" ^ (string_of_exp expr1) ^ " " ^ (string_of_exp expr2) ^ ")")
and string_of_value (v: value) : string =
  match v with
  | VLit (LInt i) -> string_of_int i
  | VLit (LBool b) -> string_of_bool b
  | VFun (e1, e2) -> ("(fun " ^ (string_of_exp e1) ^ " " ^ (string_of_exp e2) ^ ")")
  | VFix (e1, e2, e3) -> ("(fix " ^ (string_of_exp e1) ^ " " ^ (string_of_exp e2) ^ " " ^ (string_of_exp e3) ^ ")")

let rec subst (v: value) (s: string) (e: exp) : exp =
  match e with
  | EVal v' ->
    begin
      match v' with
      | VLit l -> EVal (VLit l)
      | VFun (EVar str, e') -> (if (compare str s) = 0
        then e
        else EVal (VFun (EVar str, (subst v s e'))))
      | VFix (EVar f, EVar x, e') ->
        begin
        match (compare f s), (compare x s) with
        | 0,_ -> e
        | _,0 -> e
        | _ -> EVal (VFix (EVar f, EVar x, (subst v s e')))
        end
      | _ -> failwith "Substitution: unexpected value"
    end
  | EBinOp (op, e1, e2) -> EBinOp (op, (subst v s e1 ), (subst v s e2))
  | EIf (e1, e2, e3) -> EIf ((subst v s e1), (subst v s e2), (subst v s e3))
  | EVar str -> (if (compare str s) = 0 then EVal v else e)
  | ELet (EVar str, e1, e2) -> (if (compare str s) = 0 then e
    else ELet (EVar str, (subst v s e1), (subst v s e2)))
  | EFunCall (f, e2) -> EFunCall ((subst v s f), (subst v s e2))
  | _ -> failwith "Substitution: unrecognized expression"

let typecheck_bin_op v1 v2 =
  match v1, v2 with
  | VLit (LInt _), VLit (LInt _) -> true
  | _, _ -> failwith ("Typechecking bin op failed, given: " ^ (string_of_value v1) ^ " " ^ (string_of_value v2))

let typecheck_if v =
  match v with
  | VLit (LBool _) -> true
  | _ -> false

let unpack_int_val v =
  match v with
  | VLit (LInt i) -> i
  | _ -> failwith "Error, unpack_int_val needs an int val"

let unpack_bool_val v =
  match v with
  | VLit (LBool b) -> b
  | _ -> failwith "Error, unpack_bool_val needs a bool val"

let interpret_bin_expr (op: bin_op) (v1: value) (v2: value) : value =
  let i1 = unpack_int_val v1 in
  let i2 = unpack_int_val v2 in
  match op with
  | OAdd           -> VLit (LInt (i1 + i2))
  | OSubtract      -> VLit (LInt (i1 - i2))
  | OMultiply      -> VLit (LInt (i1 * i2))
  | ODivide        -> VLit (LInt (i1 / i2))
  | OLessThanEq    -> VLit (LBool (i1 <= i2))
  | OGreaterThanEq -> VLit (LBool (i1 >= i2))
  | OLessThan      -> VLit (LBool (i1 < i2))
  | OGreaterThan   -> VLit (LBool (i1 > i2))
  | OEquals        -> VLit (LBool (i1 = i2))

let rec interpret (e: exp) : value =
  match e with
  | EVal v -> v
  | EBinOp (op, e1, e2) ->
    begin
      let v1 = interpret e1 in
      let v2 = interpret e2 in
      let typechecks = typecheck_bin_op v1 v2 in
      if not typechecks then failwith ("Typechecking failed, "
                      ^ "a binary operator should take 2 ints")
      else interpret_bin_expr op v1 v2
    end
  | EIf (e1, e2, e3) ->
    begin
      let vbool = interpret e1 in
      let typechecks = typecheck_if vbool in
      if not typechecks then failwith ("Typechecking failed, "
         ^ "an if operator should take a boolean and 2 values")
      else
      begin
        if unpack_bool_val vbool then interpret e2
        else interpret e3
      end
    end
  | ELet (EVar s, e1, e2) ->
    let s_val = interpret e1 in
    let subst_exp = subst s_val s e2 in
    interpret subst_exp
  | EVar s -> failwith ("Unbound variable: " ^ s);
  | EFunCall (e1, e2) ->
    begin
      let e1' = interpret e1 in
      match e1' with
      | VFun (EVar s, f) ->
        let s_val = interpret e2 in
        let subst_exp = subst s_val s f in
        interpret subst_exp
      | VFix (EVar f, EVar x, e) ->
        let s_input = interpret e2 in
        let subst_input = subst s_input x e in
        let subst_fname = subst e1' f subst_input in
        interpret subst_fname
      | _ -> failwith "Function call formatting incorrect"
    end
  | _ -> failwith "Interpretation: unrecognized expression"
