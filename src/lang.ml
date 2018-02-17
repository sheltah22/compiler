type lit =
  | LInt of int
  | LBool of bool

type bin_op =
  | OAdd
  | OSubtract
  | OMultiply
  | ODivide
  | OLessThanEq

type exp =
  | ELit of lit
  | EBinOp of bin_op * exp * exp
  | EIf of exp * exp * exp

let typecheck_bin_op exp1 exp2 =
  match exp1, exp2 with
    | ELit (LInt _), ELit (LInt _) -> true
    | _, _ -> false

let typecheck_if exp =
  match exp with
    | ELit (LBool _) -> true
    | _ -> false

let unpack_int_exp exp =
  match exp with
  | ELit (LInt i) -> i
  | _ -> failwith "Error, unpack_int_exp needs an int exp"

let unpack_bool_exp exp =
  match exp with
  | ELit (LBool b) -> b
  | _ -> failwith "Error, unpack_bool_exp needs a bool exp"

let interpret_bin_expr (op: bin_op) (e1: exp) (e2: exp) =
  let i1 = unpack_int_exp e1 in
  let i2 = unpack_int_exp e2 in
  match op with
  | OAdd        -> ELit (LInt (i1 + i2))
  | OSubtract   -> ELit (LInt (i1 - i2))
  | OMultiply   -> ELit (LInt (i1 * i2))
  | ODivide     -> ELit (LInt (i1 / i2))
  | OLessThanEq -> ELit (LBool (i1 <= i2))

let rec interpret (e:exp) : exp =
  match e with
    | ELit l                    -> ELit l
    | EBinOp (op, e1, e2)       ->
      begin
      let exp1 = interpret e1 in
      let exp2 = interpret e2 in
      let typechecks = typecheck_bin_op exp1 exp2 in
      if not typechecks then failwith ("Typechecking failed, "
                      ^ "a binary operator should take 2 ints")
      else interpret_bin_expr op exp1 exp2
      end
    | EIf (e1, e2, e3)          ->
      begin
      let exp1 = interpret e1 in
      let typechecks = typecheck_if exp1 in
      if not typechecks then failwith ("Typechecking failed, "
         ^ "an if operator should take a boolean and 2 values")
      else
        begin
        if unpack_bool_exp exp1 then interpret e2
        else interpret e3
        end
      end

let string_of_interpreted exp =
  match exp with
  | ELit (LInt i) -> string_of_int i
  | ELit (LBool b) -> string_of_bool b
  | _ -> failwith "Uninterpreted expression passed to string_of_interpreted"
