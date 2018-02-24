type lit =
  | LInt of int
  | LBool of bool

type bin_op =
  | OAdd
  | OSubtract
  | OMultiply
  | ODivide
  | OLessThanEq

type value =
  | VLit of lit
  | VFun of exp * exp
and exp =
  | EVal of value
  | EBinOp of bin_op * exp * exp
  | EIf of exp * exp * exp
  | EVar of string
  | ELet of exp * exp * exp
  | EFunCall of exp * exp

let rec subst (v: value) (s: string) (e: exp) : exp =
  match e with
  | EVal v ->
    begin
      match v with
      | VLit l -> EVal (VLit l)
      | VFun (e1, e2) -> EVal (VFun (e1, (subst v s e2)))
    end
  | EBinOp (op, e1, e2) -> EBinOp (op, (subst v s e1 ), (subst v s e2))
  | EIf (e1, e2, e3) -> EIf ((subst v s e1), (subst v s e2), (subst v s e3))
  | EVar str -> if (compare str s) = 0 then EVal v else e
  | ELet (str, e1, e2) -> ELet (str, (subst v s e1), (subst v s e2))
  | EFunCall (f, e2) -> EFunCall ((subst v s f), (subst v s e2))

let typecheck_bin_op v1 v2 =
  match v1, v2 with
  | VLit (LInt _), VLit (LInt _) -> true
  | _, _ -> false

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
  | OAdd        -> VLit (LInt (i1 + i2))
  | OSubtract   -> VLit (LInt (i1 - i2))
  | OMultiply   -> VLit (LInt (i1 * i2))
  | ODivide     -> VLit (LInt (i1 / i2))
  | OLessThanEq -> VLit (LBool (i1 <= i2))

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
  | EVar s -> failwith ("A VAR: " ^ s);
  | EFunCall (e1, e2) ->
    begin
      match interpret e1 with
      | VFun ((EVar s), f) ->
        let s_val = interpret e2 in
        let subst_exp = subst s_val s f in
        interpret subst_exp
      | _ -> failwith "Function call formatting incorrect"
    end

let string_of_value (v: value) : string =
  match v with
  | VLit (LInt i) -> string_of_int i
  | VLit (LBool b) -> string_of_bool b
  | VFun (e1, e2) -> "<function>"
