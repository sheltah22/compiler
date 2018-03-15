open List

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
  | TUnit
  | TPair of typ * typ
  | TList of typ
  | TRef of typ

type value =
  | VLit of lit
  | VFun of exp * exp * typ * typ
  | VFix of exp * exp * exp * typ * typ
  | VUnit
  | VPair of exp * exp
  | VEmptyList of typ
  | VCons of exp * exp
  | VPtr of int
and exp =
  | EVal of value
  | EBinOp of bin_op * exp * exp
  | EIf of exp * exp * exp
  | EVar of string
  | ELet of exp * exp * exp * typ
  | EFunCall of exp * exp
  | EFirst of exp
  | ESecond of exp
  | EHead of exp
  | ETail of exp
  | EEmpty of exp
  | ERef of exp
  | EAssign of exp * exp
  | EBang of exp
  | ESequence of exp * exp

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
  | TPair (t1, t2) -> ("(" ^ (string_of_type t1) ^ " * " ^ (string_of_type t2) ^ ")")
  | TList t -> ("[" ^ (string_of_type t) ^ "]")
  | TRef t -> ("<" ^ (string_of_type t) ^ ">")

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
  | EFirst expr -> ("(fst " ^ (string_of_exp_parsed expr) ^ ")")
  | ESecond expr -> ("(snd " ^ (string_of_exp_parsed expr) ^ ")")
  | EHead expr -> ("(head " ^ (string_of_exp_parsed expr) ^ ")")
  | ETail expr -> ("(tail " ^ (string_of_exp_parsed expr) ^ ")")
  | EEmpty expr -> ("(empty? " ^ (string_of_exp_parsed expr) ^ ")")
  | ERef expr -> ("(ref " ^ (string_of_exp_parsed expr) ^ ")")
  | EAssign (expr1, expr2) -> ("(:= " ^ (string_of_exp_parsed expr1) ^ " " ^ (string_of_exp_parsed expr2) ^ ")")
  | EBang expr -> ("(! " ^ (string_of_exp_parsed expr) ^ ")")
  | ESequence (expr1, expr2) -> ("(; " ^ (string_of_exp_parsed expr1) ^ " " ^ (string_of_exp_parsed expr2) ^ ")")
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
  | VPair (e1, e2) -> ("(" ^ (string_of_exp_parsed e1) ^ ", " ^ (string_of_exp_parsed e2) ^ ")")
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
  | EFirst expr -> ("fst " ^ (string_of_exp expr))
  | ESecond expr -> ("snd " ^ (string_of_exp expr))
  | EHead expr -> ("head " ^ (string_of_exp expr))
  | ETail expr -> ("tail " ^ (string_of_exp expr))
  | EEmpty expr -> ("empty? " ^ (string_of_exp expr))
  | ERef expr -> ("ref " ^ (string_of_exp expr))
  | EAssign (e1, e2) -> ((string_of_exp e1) ^ " := " ^ (string_of_exp e2))
  | EBang e -> ("! " ^ (string_of_exp e))
  | ESequence (e1, e2) -> ((string_of_exp e1) ^ "; " ^ (string_of_exp e2))
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
  | VPair (e1, e2) -> ("(" ^ (string_of_exp e1) ^ ", " ^ (string_of_exp e2) ^ ")")
  | VEmptyList t -> ("[] : " ^ (string_of_type t))
  | VCons (e1, e2) -> ((string_of_exp e1) ^ " :: " ^ (string_of_exp e2))
  | VPtr i -> ("(ptr, address: " ^ (string_of_int i) ^ ")")

let rec type_equals (t1: typ) (t2: typ) : bool =
  match t1, t2 with
  | TInt, TInt -> true
  | TBool, TBool -> true
  | TFun (t1, t2), TFun (t3, t4) -> (type_equals t1 t3) && (type_equals t2 t4)
  | TUnit, TUnit -> true
  | TPair (t1, t2), TPair (t3, t4) -> (type_equals t1 t3) && (type_equals t2 t4)
  | TList t1, TList t2 -> (type_equals t1 t2)
  | TRef t1, TRef t2 -> (type_equals t1 t2)
  | _ -> false

let type_of_bin_op_in (op: bin_op) : typ =
  match op with
  | _ -> TInt

let type_of_bin_op_out (op: bin_op) : typ =
  match op with
  | OAdd | OSubtract
  | OMultiply | ODivide -> TInt
  | _ -> TBool

let rec typecheck (ctx: (string * typ) list) (e: exp) : typ =
  match e with
  | ESequence (e1, e2) ->
    let e1_type = (typecheck ctx e1) in (typecheck ctx e2)
  | EVal (VLit (LInt _)) -> TInt
  | EVal (VLit (LBool _)) -> TBool
  | EVal (VFun (EVar s, e', t1, t2)) ->
    let e_type = typecheck (cons (s, t1) ctx) e' in
    if type_equals e_type t2 then TFun (t1, t2)
    else failwith ("Function typechecking failed, expected return type: "
    ^ (string_of_type (TFun (t1, t2))) ^ ", actual: " ^ (string_of_type (TFun (t1, e_type))))
  | EVal (VFix (EVar f, EVar s, e, t1, t2)) ->
    let e_type = typecheck (cons (f, TFun(t1,t2)) (cons (s, t1) ctx)) e in
    if type_equals e_type t2 then TFun (t1, t2)
    else failwith ("Fixpoint typechecking failed, expected return type: "
    ^ (string_of_type (TFun (t1, t2))) ^ ", actual: " ^ (string_of_type (TFun (t1, e_type))))
  | EVal (VUnit) -> TUnit
  | EVal (VPair (e1, e2)) -> TPair ((typecheck ctx e1), (typecheck ctx e2))
  | EVal (VEmptyList t) -> TList t
  | EVal (VCons (e1, e2)) ->
    begin
    let e1_type = (typecheck ctx e1) in
    let e2_type = (typecheck ctx e2) in
    match e1_type, e2_type with
    | t1, TList t2 -> (if (type_equals t1 t2) then TList t1
      else failwith ("Cons typechecking failed, e1 should have type of contents"
      ^ " of e2, actual: " ^ (string_of_type e1_type) ^ ", " ^ (string_of_type e2_type)))
    | _ -> failwith ("Cons typechecking failed, e2 should have type list, "
      ^ "actual: " ^ (string_of_type e2_type))
    end
  | EVal (VPtr i) -> failwith("Typechecking error: should not have encountered a pointer value")
  | EBinOp (op, e1, e2) ->
    let in_type = type_of_bin_op_in op in
    let e1_type = typecheck ctx e1 in
    let e2_type = typecheck ctx e2 in
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
    ^ (string_of_type t) ^ ", actual: " ^ (string_of_type e1_type))
  | EFunCall (e1, e2) ->
    begin
    let e1_type = typecheck ctx e1 in
    let e2_type = typecheck ctx e2 in
    match e1_type with
    | TFun (t1, t2) ->
      begin
        if type_equals t1 e2_type then t2
        else failwith ("Fun call typechecking failed, expected input type: "
        ^ (string_of_type t1) ^ ", actual: " ^ (string_of_type e2_type) ^ (string_of_exp e1) ^ " " ^ (string_of_exp e2))
      end
    | _ -> failwith ("Fun call typechecking error, first expression should be"
      ^ " of type function, actual: " ^ (string_of_type e1_type))
    end
  | EFirst ex ->
    begin
    let e_type = (typecheck ctx ex) in
    match e_type with
    | TPair (t1, t2) -> t1
    | _ -> failwith ("First typechecking failed, exp should be of type pair,"
      ^ " actual: " ^ (string_of_type e_type))
    end
  | ESecond ex ->
    begin
    let e_type = (typecheck ctx ex) in
    match e_type with
    | TPair (t1, t2) -> t2
    | _ -> failwith ("Second typechecking failed, exp should be of type pair,"
      ^ " actual: " ^ (string_of_type e_type))
    end
  | EHead ex ->
    begin
    let e_type = (typecheck ctx ex) in
    match e_type with
    | TList t -> t
    | _ -> failwith ("Head typechecking failed, exp should be of type list, "
      ^ "actual: " ^ (string_of_type e_type))
    end
  | ETail ex ->
    begin
    let e_type = (typecheck ctx ex) in
    match e_type with
    | TList t -> TList t
    | _ -> failwith ("Tail typechecking failed, exp should be of type list, "
      ^ "actual: " ^ (string_of_type e_type))
    end
  | EEmpty ex ->
    begin
    let e_type = (typecheck ctx ex) in
    match e_type with
    | TList t -> TBool
    | _ -> failwith ("empty? typechecking failed, exp should be of type list, "
      ^ "actual: " ^ (string_of_type e_type))
    end
  | ERef ex -> TRef (typecheck ctx ex)
  | EAssign (e1, e2) ->
    begin
    let e1_type = (typecheck ctx e1) in
    let e2_type = (typecheck ctx e2) in
    match e1_type, e2_type with
    | TRef t1, t2 -> if (type_equals t1 t2) then TUnit
      else failwith ("Assignment typechecking failed, type of ref should be same as "
      ^ "type of value, actual: " ^ (string_of_type t1) ^ ", " ^ (string_of_type t2))
    | t, _ -> failwith ("Assignment typechecking failed, first value should "
      ^ "have type <t>, actual: " ^ (string_of_type t))
    end
  | EBang ex ->
    begin
    let e_type = (typecheck ctx ex) in
    match e_type with
    | TRef t -> t
    | _ -> failwith ("Bang typechecking failed, should take ref type, actual "
      ^ (string_of_type e_type))
    end
  | _ -> failwith ("Typechecking failed, unrecognized formatting" ^ (string_of_exp e))

let rec subst (v: value) (s: string) (e: exp) : exp =
  match e with
  | EVal v' ->
    begin
      match v' with
      | VLit l -> e
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
      | VUnit -> e
      | VPair (e1, e2) -> EVal (VPair ((subst v s e1), (subst v s e2)))
      | VEmptyList t -> EVal (VEmptyList t)
      | VCons (e1, e2) -> EVal (VCons ((subst v s e1), (subst v s e2)))
      | VPtr i -> e
      | _ -> failwith "Substitution: unexpected value"
    end
  | EBinOp (op, e1, e2) -> EBinOp (op, (subst v s e1 ), (subst v s e2))
  | EIf (e1, e2, e3) -> EIf ((subst v s e1), (subst v s e2), (subst v s e3))
  | EVar str -> (if (compare str s) = 0 then EVal v else e)
  | ELet (EVar str, e1, e2, t) -> (if (compare str s) = 0 then e
    else ELet (EVar str, (subst v s e1), (subst v s e2), t))
  | EFunCall (f, e2) -> EFunCall ((subst v s f), (subst v s e2))
  | EFirst ex -> EFirst (subst v s ex)
  | ESecond ex -> ESecond (subst v s ex)
  | EHead ex -> EHead (subst v s ex)
  | ETail ex -> ETail (subst v s ex)
  | EEmpty ex -> EEmpty (subst v s ex)
  | ERef ex -> ERef (subst v s ex)
  | EAssign (e1, e2) -> EAssign ((subst v s e1), (subst v s e2))
  | EBang ex -> EBang (subst v s ex)
  | ESequence (e1, e2) -> ESequence ((subst v s e1), (subst v s e2))
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

let rec is_value (e: exp) : bool =
  match e with
  | EVal (VPair (e1, e2)) -> (is_value e1) && (is_value e2)
  | EVal (VCons (e1, e2)) -> (is_value e1) && (is_value e2)
  | EVal _ -> true
  | _ -> false

let exp_to_value (e: exp) : value =
  match e with
  | EVal v -> v
  | _ -> failwith ("exp_to_value called with non-value expression: " ^ (string_of_exp e))

let rec step (env: (int * value) list) (e: exp) : (int * value) list * exp =
  match e with
  | ESequence (e1, e2) ->
    let interp_e1 = not (is_value e1) in
    if interp_e1 then
    begin
      match (step env e1) with
      | env', e1' -> (env', ESequence (e1', e2))
    end
    else (env, e2)
  | EVal (VPair (e1, e2)) ->
    let interp_e1 = not (is_value e1) in
    let interp_e2 = not (is_value e2) in
    if interp_e1 then
      let (env', e1') = (step env e1) in (env', EVal (VPair (e1', e2)))
    else if interp_e2 then
      let (env', e2') = (step env e2) in (env', EVal (VPair (e1, e2')))
    else (env, e)
  | EVal (VCons (e1, e2)) ->
    let interp_e1 = not (is_value e1) in
    let interp_e2 = not (is_value e2) in
    if interp_e1 then
      let (env', e1') = (step env e1) in (env', EVal (VCons (e1', e2)))
    else if interp_e2 then
      let (env', e2') = (step env e2) in (env', EVal (VCons (e1, e2')))
    else (env, e)
  | EVal v -> (env, e)
  | EBinOp (op, e1, e2) ->
    begin
      let interp_e1 = not (is_value e1) in
      let interp_e2 = not (is_value e2) in
      match interp_e1, interp_e2 with
      | true, _ ->
      begin
        match (step env e1) with
        | env', e1' -> (env', EBinOp (op, e1', e2))
      end
      | false, true ->
      begin
        match (step env e2) with
        | env', e2' -> (env', EBinOp (op, e1, e2'))
      end
      | false, false -> (env, EVal (interpret_bin_exp op (exp_to_value e1) (exp_to_value e2)))
    end
  | EIf (e1, e2, e3) ->
    begin
      let interp_e1 = not (is_value e1) in
      match interp_e1 with
      | true ->
      begin
        match (step env e1) with
        | env', e1' -> (env', EIf (e1', e2, e3))
      end
      | false -> if (unpack_bool_val (exp_to_value e1))
        then (step env e2) else (step env e3)
    end
  | ELet (EVar s, e1, e2, t) ->
    begin
      let interp_e1 = not (is_value e1) in
      if interp_e1 then
      begin
        match (step env e1) with
        | env', e1' -> (env', ELet ((EVar s), e1', e2, t))
      end
      else (env, (subst (exp_to_value e1) s e2))
    end
  | EVar s -> failwith ("Unbound variable binding: " ^ s);
  | EFunCall (e1, e2) ->
    begin
    let interp_e2 = not (is_value e2) in
    if interp_e2 then
    begin
      match (step env e2) with
      | env', e2' -> (env', EFunCall (e1, e2'))
    end
    else
      match e1 with
      | EFunCall _ ->
        begin
        match (step env e1) with
        | env', e1' -> (env', EFunCall (e1', e2))
        end
      | EVal (VFun (EVar s, f, t1, t2)) -> (env, subst (exp_to_value e2) s f)
      | EVal (VFix (EVar f, EVar x, e, t1, t2)) ->
        let subst_x = subst (exp_to_value e2) x e in
        (env, subst (exp_to_value e1) f subst_x)
      | _ -> failwith ("Interpretation: issue with function formatting")
    end
  | EFirst ex ->
    let interp_ex = not (is_value ex) in
    if interp_ex then
    begin
      match (step env ex) with
      | env', ex' -> (env', EFirst ex')
      end
    else
    begin
      match (exp_to_value ex) with
      | VPair (e1, e2) -> (env, e1)
      | _ -> failwith ("Typechecking missed fst")
    end
  | ESecond ex ->
    let interp_ex = not (is_value ex) in
    if interp_ex then
    begin
      match (step env ex) with
      | env', ex' -> (env', ESecond ex')
      end
    else
    begin
      match (exp_to_value ex) with
      | VPair (e1, e2) -> (env, e2)
      | _ -> failwith ("Typechecking missed snd")
    end
  | EHead ex ->
    let interp_ex = not (is_value ex) in
    if interp_ex then
    begin
      match (step env ex) with
      | env', ex' -> (env', EHead ex')
      end
    else
    begin
      match (exp_to_value ex) with
      | VEmptyList _ -> failwith ("Attempted to take hd of empty list")
      | VCons (e1, e2) -> (env, e1)
      | _ -> failwith ("Typechecking missed hd")
    end
  | ETail ex ->
    let interp_ex = not (is_value ex) in
    if interp_ex then
    begin
      match (step env ex) with
      | env', ex' -> (env', ETail ex')
      end
    else
    begin
      match (exp_to_value ex) with
      | VEmptyList _ -> failwith ("Attempted to take tl of empty list")
      | VCons (e1, e2) -> (env, e2)
      | _ -> failwith ("Typechecking missed tl")
    end
  | EEmpty ex ->
    let interp_ex = not (is_value ex) in
    if interp_ex then
    begin
      match (step env ex) with
      | env', ex' -> (env', EEmpty ex')
      end
    else
    begin
      match (exp_to_value ex) with
      | VEmptyList _ -> (env, EVal (VLit (LBool true)))
      | VCons (e1, e2) -> (env, EVal (VLit (LBool false)))
      | _ -> failwith ("Typechecking missed empty?")
    end
  | ERef ex ->
    begin
    let interp_ex = not (is_value ex) in
    if (interp_ex) then
    begin
      match (step env ex) with
      | env', ex' -> (env', ERef ex')
    end
    else
    let new_i = (length env) in
    ((cons (new_i, (exp_to_value ex)) env), EVal (VPtr new_i))
    end
  | EAssign (e1, e2) ->
    let interp_e1 = not (is_value e1) in
    let interp_e2 = not (is_value e2) in
    if interp_e1 then
    begin
      match (step env e1) with
      | env', e1' -> (env', EAssign (e1', e2))
    end
    else
    begin
      if interp_e2 then
        begin
          match (step env e2) with
          | env', e2' -> (env', EAssign (e1, e2'))
          end
      else
        begin
          match ((exp_to_value e1), (exp_to_value e2)) with
          | VPtr i, v -> ((cons (i, v) env), EVal (VUnit))
          | v, _ -> failwith("Typechecking missed :=, actual: " ^ (string_of_exp e1))
        end
    end
  | EBang ex ->
    let interp_ex = not (is_value ex) in
    if (interp_ex) then
    begin
      match (step env ex) with
      | env', ex' -> (env', EBang ex')
    end
    else
    begin
      match (exp_to_value ex) with
      | VPtr i -> (env, EVal (assoc i env))
      | _ -> failwith ("Typechecking missed !")
    end
  | _ -> failwith "Interpretation: unrecognized expression"

let eval (expr: exp) : value =
  let rec eval' (env: (int * value) list) (e: exp) : exp =
    if is_value e then e else
    begin
    match (step env e) with
    | env', e' -> (eval' env' e')
    end in
  let typechecks = typecheck [] expr in
  exp_to_value (eval' [] expr)

let eval_print (expr: exp) : unit =
  let rec eval' (env: (int * value) list) (e: exp) : exp =
    if is_value e then e
    else
      (let env', result = (step env e) in
      print_endline ("--> " ^ (string_of_exp result));
      (eval' env' result)) in
  let typechecks = typecheck [] expr in
  print_endline ("    " ^ string_of_exp expr);
  eval' [] expr; ();
