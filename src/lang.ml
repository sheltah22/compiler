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

type typ =
  | TInt
  | TBool
  | TFun of typ * typ

type value =
  | VLit of lit
  | VFun of exp * exp * typ * typ
  | VFix of exp * exp * exp * typ * typ
and exp =
  | EVal of value
  | EBinOp of bin_op * exp * exp
  | EIf of exp * exp * exp
  | EVar of string
  | ELet of exp * exp * exp * typ
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

let rec string_of_type (t: typ) : string =
  match t with
  | TInt -> "int"
  | TBool -> "bool"
  | TFun (t1, t2) -> ((string_of_type t1) ^ " -> " ^ (string_of_type t2))

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
and string_of_value_parsed (v: value) : string =
  match v with
  | VLit (LInt i) -> string_of_int i
  | VLit (LBool b) -> string_of_bool b
  | VFun (e1, e2, t1, t2) -> ("(fun (" ^ (string_of_exp_parsed e1) ^ ": "
    ^ (string_of_type t1) ^ ") : " ^ (string_of_type t2) ^ " -> "
    ^ (string_of_exp_parsed e2) ^ ")")
  | VFix (e1, e2, e3, t1, t2) -> ("(fix " ^ (string_of_exp_parsed e1) ^ " ( "
    ^ (string_of_exp_parsed e2) ^ ": " ^ (string_of_type t1) ^ ") : "
    ^ (string_of_type t2) ^ " -> " ^ " " ^ (string_of_exp_parsed e3) ^ ")")

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
and string_of_value (v: value) : string =
  match v with
  | VLit (LInt i) -> string_of_int i
  | VLit (LBool b) -> string_of_bool b
  | VFun (e1, e2, t1, t2) -> ("fun (" ^ (string_of_exp e1) ^ ": "
    ^ (string_of_type t1) ^ ") : " ^ (string_of_type t2) ^ " -> " ^ (string_of_exp e2))
  | VFix (e1, e2, e3, t1, t2) -> ("fix (" ^ (string_of_exp e1) ^ ": "
    ^ (string_of_type t2) ^ ") :" ^ (string_of_type t2) ^ (string_of_exp e2) ^ " -> "
    ^ (string_of_exp e3))

let type_equals (t1: typ) (t2: typ) : bool =
  match t1, t2 with
  | TInt, TInt -> true
  | TBool, TBool -> true
  | TFun (t1, t2), TFun (t1, t2) -> true
  | _ -> false

let type_of_bin_op_in (op: bin_op) : typ =
  match op with
  | _ -> TInt

let type_of_bin_op_out (op: bin_op) : typ =
  match op with
  | OAdd | OSubtract
  | OMultiply | ODivide -> TInt
  | _ -> TBool

let rec typecheck (ctx: string * typ list) (e: exp) : typ =
  match e with
  | EVal (VLit (LInt _)) -> TInt
  | EVal (VLit (LBool _)) -> TBool
  | EVal (VFun (EVar s, e, t1, t2)) ->
    let e_type = typecheck (cons (s, t1) ctx) e in
    if type_equals e_type t2 then t2
    else failwith ("Function typechecking failed, expected return type: "
    ^ (string_of_type t2) ^ ", actual: " ^ (string_of_type e_type))
  | EVal (VFun (EVar f, EVar s, e, t1, t2)) ->
    let e_type = typecheck (cons (f, t2) (cons (s, t1) ctx)) in
    if type_equals e_type t2 then t2
    else failwith ("Fixpoint typechecking failed, expected return type: "
    ^ (string_of_type t2) ^ ", actual: " ^ (string_of_type e_type))
  | EBinOp (op, e1, e2) ->
    let in_type = type_of_bin_op_in op in
    let e1_type = typecheck e1 in
    let e2_type = typecheck e2 in
    if (type_equals e1_type in_type) && (type_equals e2_type in_type) then type_of_bin_op_out op
    else failwith ("Binary op typechecking failed, expected input type: "
    ^ (string_of_type in_type) ^ ", actual: " ^ (string_of_type e1_type)
    ^ " and " ^ (string_of_type e2_type))
  | EIf (e1, e2, e3) ->
    let e1_type = typecheck ctx e1 in
    let e2_type = typecheck ctx e2 in
    let e3_type = typecheck ctx e3 in
    if (type_equals e1_type TBool) && (type_equals e2_type e3_type)
    then e2_type
    else failwith
    ("If typechecking failed, expected format: if <bool> then <t> else <t>, "
    ^ "actual: " ^ (string_of_type e1_type) ^ ", " ^ (string_of_type e2_type)
    ^ ", " ^ (string_of_type e3_type))
  | EVar s -> List.assoc s ctx
  | ELet (EVar s, e1, e2, t) ->
    let e1_type = typecheck ctx e1 in
    let e2_type = typecheck (cons (s, t) ctx) e2 in
    if (type_equals t e1_type) then e2_type
    else failwith ("Let typechecking failed, expected binding type: "
    ^ (string_of_type t) ", actual: " ^ (string_of_type e1_type))
  | EFunCall (e1, e2) ->
    let e1_type = typecheck ctx e1 in
    let e2_type = typecheck ctx e2 in
    match e1_type with
    | TFun (t1, t2) ->
      begin
        if type_equals t1 e2_type then t2
        else failwith ("Fun call typechecking failed, expected input type: "
        ^ (string_of_type t1) ^ ", actual: " ^ (string_of_type e2_type))
      end
    | _ -> failwith ("Fun call typechecking error, first expression should be"
      ^ " of type function, actual: " ^ (string_of_type e1_type))

let rec subst (v: value) (s: string) (e: exp) : exp =
  match e with
  | EVal v' ->
    begin
      match v' with
      | VLit l -> EVal (VLit l)
      | VFun (EVar str, e', t1, t2) -> (if (compare str s) = 0
        then e
        else EVal (VFun (EVar str, (subst v s e'), t1, t2)))
      | VFix (EVar f, EVar x, e', t1, t2) ->
        begin
        match (compare f s), (compare x s) with
        | 0,_ -> e
        | _,0 -> e
        | _ -> EVal (VFix (EVar f, EVar x, (subst v s e'), t1, t2))
        end
      | _ -> failwith "Substitution: unexpected value"
    end
  | EBinOp (op, e1, e2) -> EBinOp (op, (subst v s e1 ), (subst v s e2))
  | EIf (e1, e2, e3) -> EIf ((subst v s e1), (subst v s e2), (subst v s e3))
  | EVar str -> (if (compare str s) = 0 then EVal v else e)
  | ELet (EVar str, e1, e2, t) -> (if (compare str s) = 0 then e
    else ELet (EVar str, (subst v s e1), (subst v s e2), t))
  | EFunCall (f, e2) -> EFunCall ((subst v s f), (subst v s e2))
  | _ -> failwith "Substitution: unrecognized expression"

let unpack_int_val v =
  match v with
  | VLit (LInt i) -> i
  | _ -> failwith "Error, unpack_int_val needs an int val"

let unpack_bool_val v =
  match v with
  | VLit (LBool b) -> b
  | _ -> failwith "Error, unpack_bool_val needs a bool val"

let interpret_bin_exp (op: bin_op) (v1: value) (v2: value) : value =
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

let is_value (e: exp) : bool =
  match e with
  | EVal _ -> true
  | _ -> false

let exp_to_value (e: exp) : value =
  match e with
  | EVal v -> v
  | _ -> failwith ("exp_to_value called with non-value expression: " ^ (string_of_exp e))

let rec step (e: exp) : exp =
  match e with
  | EVal v -> e
  | EBinOp (op, e1, e2) ->
    begin
      let interp_e1 = not (is_value e1) in
      let interp_e2 = not (is_value e2) in
      match interp_e1, interp_e2 with
      | true, _ -> EBinOp (op, step e1, e2)
      | false, true -> EBinOp (op, e1, step e2)
      | false, false -> EVal (interpret_bin_exp op (exp_to_value e1) (exp_to_value e2))
    end
  | EIf (e1, e2, e3) ->
    begin
      let interp_e1 = not (is_value e1) in
      match interp_e1 with
      | true -> EIf (step e1, e2, e3)
      | false -> if (unpack_bool_val (exp_to_value e1)) then (step e2) else (step e3)
    end
  | ELet (EVar s, e1, e2, t) ->
    begin
      let interp_e1 = not (is_value e1) in
      if interp_e1 then ELet (EVar s, step e1, e2, t)
      else subst (exp_to_value e1) s e2
    end
  | EVar s -> failwith ("Unbound variable: " ^ s);
  | EFunCall (e1, e2) ->
    begin
    let interp_e2 = not (is_value e2) in
    if interp_e2 then EFunCall (e1, step e2)
    else
      match e1 with
      | EFunCall _ -> EFunCall (step e1, e2)
      | EVal (VFun (EVar s, f, t1, t2)) -> subst (exp_to_value e2) s f
      | EVal (VFix (EVar f, EVar x, e, t1, t2)) ->
        let subst_x = subst (exp_to_value e2) x e in
        subst (exp_to_value e1) f subst_x
      | _ -> failwith ("Interpretation: issue with function formatting")
    end
  | _ -> failwith "Interpretation: unrecognized expression"

let eval (expr: exp) : value =
  let rec eval' (e: exp) : exp =
    if is_value e then e else eval'(step e) in
  let typechecks = typecheck [] expr in
  exp_to_value (eval' expr)

let eval_print (expr: exp) : unit =
  let rec eval' (e: exp) : exp =
    if is_value e then e
    else
      (let result = step e in
      print_endline ("--> " ^ (string_of_exp result));
      eval'(result)) in
  let typechecks = typecheck [] expr in
  print_endline ("    " ^ string_of_exp expr);
  eval' expr; ();
