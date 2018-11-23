(* lang.ml *)
(* functions for typechecking, evaluation *)
open List
open Exps
open Display

type constraint =
  | CEquals of typ * typ

let rec type_equals (t1: typ) (t2: typ) : bool =
  match t1, t2 with
  | TInt, TInt -> true
  | TBool, TBool -> true
  | TFun (t1, t2), TFun (t3, t4) -> (type_equals t1 t3) && (type_equals t2 t4)
  | TUnit, TUnit -> true
  | TTuple [], TTuple [] -> true
  | TTuple (t1 :: rest1), TTuple (t2 :: rest2) -> (type_equals t1 t2) && (type_equals (TTuple rest1) (TTuple rest2))
  | TList t1, TList t2 -> (type_equals t1 t2)
  | TRef t1, TRef t2 -> (type_equals t1 t2)
  | _ -> false

let type_of_bin_op_in (op: bin_op) : typ =
  match op with
  | _ -> TInt

let type_of_bin_op_out (op: bin_op) : typ =
  match op with
  | OAdd | OSubtract | OMultiply | ODivide -> TInt
  | _ -> TBool

let unpack_int_val v =
  match v with
  | VLit (LInt i) -> i
  | _ -> failwith "Error, unpack_int_val needs an int val"

let unpack_bool_val v =
  match v with
  | VLit (LBool b) -> b
  | _ -> failwith "Error, unpack_bool_val needs a bool val"

let exp_to_value (e: exp) : value =
  match e with
  | EVal v -> v
  | _ -> failwith ("exp_to_value called with non-value expression: " ^ (string_of_exp e))

let rec typecheck (ctx: (string * typ) list) (e: exp) (i: int) : typ * constraint list * i =
  match e with
  | ESequence (e1, e2) ->
    let (t1, cs1, i1) = (typecheck ctx e1 i) in
    let (t2, cs2, i2) = (typecheck ctx e2 i1) in
    (t2, cs1 @ cs2, i2)
  | EVal (VLit (LInt _)) -> (TInt, i, [])
  | EVal (VLit (LBool _)) -> (TBool, i, [])
  | EVal (VFun (EVar s, e', t1, t2)) ->
    let (cs, e_type, i1) = typecheck (cons (s, t1) ctx) e' i in
    if type_equals e_type t2 then (TFun (t1, t2), (CEquals e_type t2) :: cs, i1)
    else failwith ("Function typechecking failed, expected return type: "
    ^ (string_of_type (TFun (t1, t2))) ^ ", actual: " ^ (string_of_type (TFun (t1, e_type))))
  | EVal (VFix (EVar f, EVar s, e, t1, t2)) ->
    let (cs, e_type, i1) = typecheck (cons (f, TFun(t1,t2)) (cons (s, t1) ctx)) e i in
    if type_equals e_type t2 then (TFun (t1, t2), (CEquals e_type t2) :: cs, i1)
    else failwith ("Fixpoint typechecking failed, expected return type: "
    ^ (string_of_type (TFun (t1, t2))) ^ ", actual: " ^ (string_of_type (TFun (t1, e_type))))
  | EVal (VUnit) -> (TUnit, (), i)
  | EVal (VTuple []) -> (TTuple ([]), [], i)
  | EVal (VTuple (ex :: rest)) ->
    begin
      match (typecheck ctx (EVal (VTuple rest)) i) with
      | (TTuple rest', cs2, i1) ->
        let (t1, cs1, i2) = (typecheck ctx ex i1) in
        (TTuple (t1 :: rest'), cs1 @ cs2, i2)
      | _ -> failwith "Typechecking missed tuples"
    end
  | EVal (VEmptyList t) -> (TList t, [], i)
  | EVal (VCons (e1, e2)) ->
    begin
    let (e1_type, cs1, i1) = (typecheck ctx e1 i) in
    let (e2_type, cs2, i2) = (typecheck ctx e2 i1) in
    match e1_type, e2_type with
    | t1, TList t2 -> (if (type_equals t1 t2) then (TList t1, (CEquals t1 t2) :: (cs1 @ cs2), i2)
      else failwith ("Cons typechecking failed, e1 should have type of contents"
      ^ " of e2, actual: " ^ (string_of_type e1_type) ^ ", " ^ (string_of_type e2_type)))
    | _ -> failwith ("Cons typechecking failed, e2 should have type list, "
      ^ "actual: " ^ (string_of_type e2_type))
    end
  | EVal (VPtr i) -> failwith("Typechecking error: should not have encountered a pointer value")
  | EBinOp (op, e1, e2) ->
    let in_type = type_of_bin_op_in op in
    let (e1_type, cs1, i1) = typecheck ctx e1 i in
    let (e2_type, cs2, i2) = typecheck ctx e2 i1 in
    if (type_equals e1_type in_type) && (type_equals e2_type in_type)
    then (type_of_bin_op_out op, (CEquals e1_type in_type) :: (CEquals e2_type in_type) :: (cs1 @ cs2), i2)
    else failwith ("Binary op typechecking failed, expected input type: "
    ^ (string_of_type in_type) ^ ", actual: " ^ (string_of_type e1_type)
    ^ " and " ^ (string_of_type e2_type))
  | EIf (e1, e2, e3) ->
    let (e1_type, cs1, i1) = typecheck ctx e1 i in
    let (e2_type, cs2, i2)  = typecheck ctx e2 i1 in
    let (e3_type, cs3, i3) = typecheck ctx e3 i2 in
    if (type_equals e1_type TBool) && (type_equals e2_type e3_type)
    then (e2_type, (CEquals e1_type TBool) :: (CEquals e2_type e3_type) :: (cs1 @ cs2 @ cs3), i3)
    else failwith
    ("If typechecking failed, expected format: if <bool> then <t> else <t>, "
    ^ "actual: " ^ (string_of_type e1_type) ^ ", " ^ (string_of_type e2_type)
    ^ ", " ^ (string_of_type e3_type))
  | EVar s -> (List.assoc s ctx, [], i)
  | ELet (EVar s, e1, e2, t) ->
    let (e1_type, cs1, i1) = typecheck ctx e1 i in
    let (e2_type, cs2, i2) = typecheck (cons (s, t) ctx) e2 i1 in
    if (type_equals t e1_type) then (e2_type, (CEquals t e1_type) :: cs1 @ cs2, i2)
    else failwith ("Let typechecking failed, expected binding type: "
    ^ (string_of_type t) ^ ", actual: " ^ (string_of_type e1_type))
  | EFunCall (e1, e2) ->
    begin
    let (e1_type, cs1, i1) = typecheck ctx e1 i in
    let (e2_type, cs2, i2) = typecheck ctx e2 i1 in
    match e1_type with
    | TFun (t1, t2) ->
      begin
        if type_equals t1 e2_type then (t2, (CEquals t1 e2_type) :: cs1 @ cs2, i2)
        else failwith ("Fun call typechecking failed, expected input type: "
        ^ (string_of_type t1) ^ ", actual: " ^ (string_of_type e2_type) ^ (string_of_exp e1) ^ " " ^ (string_of_exp e2))
      end
    | _ -> failwith ("Fun call typechecking error, first expression should be"
      ^ " of type function, actual: " ^ (string_of_type e1_type))
    end
  | EHead ex ->
    begin
    let (e_type, cs, i1) = typecheck ctx ex i in
    match e_type with
    | TList t -> (t, cs, i1)
    | _ -> failwith ("Head typechecking failed, exp should be of type list, "
      ^ "actual: " ^ (string_of_type e_type))
    end
  | ETail ex ->
    begin
    let (e_type, cs, i1) = typecheck ctx ex i in
    match e_type with
    | TList t -> (TList t, cs, i1)
    | _ -> failwith ("Tail typechecking failed, exp should be of type list, "
      ^ "actual: " ^ (string_of_type e_type))
    end
  | EEmpty ex ->
    begin
    let (e_type, cs, i1) = typecheck ctx ex i in
    match e_type with
    | TList t -> (TBool, cs, i1)
    | _ -> failwith ("empty? typechecking failed, exp should be of type list, "
      ^ "actual: " ^ (string_of_type e_type))
    end
  | ERef ex ->
    let (e_type, cs, i1) = typecheck ctx ex i in
    (TRef e_type, cs, i1)
  | EAssign (e1, e2) ->
    begin
    let (e1_type, cs1, i1) = typecheck ctx e1 i in
    let (e2_type, cs2, i2) = typecheck ctx e2 i1 in
    match e1_type, e2_type with
    | TRef t1, t2 -> if (type_equals t1 t2)
      then (TUnit, (CEquals t1 t2) :: cs1 @ cs2, i2)
      else failwith ("Assignment typechecking failed, type of ref should be same as "
      ^ "type of value, actual: " ^ (string_of_type t1) ^ ", " ^ (string_of_type t2))
    | t, _ -> failwith ("Assignment typechecking failed, first value should "
      ^ "have type <t>, actual: " ^ (string_of_type t))
    end
  | EBang ex ->
    begin
    let (e_type, cs, i1) = typecheck ctx ex i in
    match e_type with
    | TRef t -> (t, cs, i1)
    | _ -> failwith ("Bang typechecking failed, should take ref type, actual "
      ^ (string_of_type e_type))
    end
  | EWhile (e1, e2, e3) ->
    begin
    let (e1_type, cs, i1) = typecheck ctx e1 i in
    match e1_type with
    | TBool -> (TUnit, cs, i1)
    | _ -> failwith ("While typechecking failed, e1 should be of type bool, "
      ^ "actual: " ^ (string_of_type e1_type))
    end
  | ENth (e1, e2) ->
    let (e2_type, cs1, i1) = typecheck ctx e2 i in
    if (not (type_equals e2_type TInt)) then (failwith ("Nth typechecking failed, expects int type second"))
    else
    let (e1_type, cs2, i2) = typecheck ctx e1 i1 in
    begin
     match e1_type with
      | TTuple l -> ((nth l (unpack_int_val (exp_to_value e2))),
        (CEquals e2_type TInt) :: cs1 @ cs2, i2)
      | t -> failwith ("Nth typechecking failed, should take tuple first, "
        ^ "actual: " ^ (string_of_type t))
    end
  | EInferLet (EVar s, e1, e2) ->
    let (e1_type, cs1, i1) = typecheck ctx e1 i in
    let (e2_type, cs2, i2) = typecheck (cons (s, e1_type) ctx) e2 i1 in
    (e2_type, cs1 @ cs2, i2)
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
      | VTuple [] -> EVal (VTuple [])
      | VTuple (ex :: rest) ->
        begin
          match (subst v s (EVal (VTuple rest))) with
          | EVal (VTuple rest') -> EVal (VTuple ((subst v s ex) :: rest'))
          | _ -> failwith "Tuple substitution unexpected value"
        end
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
  | EHead ex -> EHead (subst v s ex)
  | ETail ex -> ETail (subst v s ex)
  | EEmpty ex -> EEmpty (subst v s ex)
  | ERef ex -> ERef (subst v s ex)
  | EAssign (e1, e2) -> EAssign ((subst v s e1), (subst v s e2))
  | EBang ex -> EBang (subst v s ex)
  | ESequence (e1, e2) -> ESequence ((subst v s e1), (subst v s e2))
  | EWhile (e1, e2, e3) -> EWhile ((subst v s e1), (subst v s e2), (subst v s e3))
  | ENth (e1, e2) -> (ENth ((subst v s e1), (subst v s e2)))
  | EInferLet (EVar str, e1, e2) -> (if (compare str s) = 0 then e
    else EInferLet (EVar str, (subst v s e1), (subst v s e2)))
  | _ -> failwith "Substitution: unrecognized expression"

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
  | EVal (VTuple []) -> true
  | EVal (VTuple (ex :: rest)) -> (is_value ex) && (is_value (EVal (VTuple rest)))
  | EVal (VCons (e1, e2)) -> (is_value e1) && (is_value e2)
  | EVal _ -> true
  | _ -> false

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
  | EVal (VTuple []) -> (env, e)
  | EVal (VTuple (ex :: rest)) ->
    let interp_e1 = not (is_value ex) in
    if interp_e1 then
      let (env', ex') = (step env ex) in (env', EVal (VTuple (ex' :: rest)))
    else
    begin
      match (step env (EVal (VTuple rest))) with
      | env', EVal (VTuple rest') -> (env', EVal (VTuple (ex :: rest')))
      | _, _ -> failwith "Tuple didn't evaluate to a tuple"
    end
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
  | EWhile (e1, e2, e3) ->
    let interp_e1 = not (is_value e1) in
    if (interp_e1) then
    begin
      match (step env e1) with
      | env', e1' -> (env', EWhile (e1', e2, e3))
    end
    else
    begin
      match (exp_to_value e1) with
      | VLit (LBool true) -> (env, ESequence(e3, EWhile (e2, e2, e3)))
      | VLit (LBool false) -> (env, EVal (VUnit))
      | _ -> failwith ("Typechecking missed while")
    end
  | ENth (e1, e2) ->
    let interp_e1 = not (is_value e1) in
    let interp_e2 = not (is_value e2) in
    if interp_e1 then
    let (env', e1') = (step env e1) in (env', (ENth (e1', e2)))
    else
    begin
      if interp_e2 then let (env', e2') = (step env e2) in (env', (ENth (e1, e2')))
      else
      let n = (unpack_int_val (exp_to_value e2)) in
      begin
      match e1 with
      | EVal (VTuple l) -> (env, (nth l n))
      | _ -> failwith ("Typechecking missed nth")
      end
    end
  | EInferLet (EVar s, e1, e2) ->
    begin
      let interp_e1 = not (is_value e1) in
      if interp_e1 then
      begin
        match (step env e1) with
        | env', e1' -> (env', EInferLet ((EVar s), e1', e2))
      end
      else (env, (subst (exp_to_value e1) s e2))
    end
  | _ -> failwith "Interpretation: unrecognized expression"

let eval (expr: exp) : value =
  let rec eval' (env: (int * value) list) (e: exp) : exp =
    if is_value e then e else
    begin
    match (step env e) with
    | env', e' -> (eval' env' e')
    end in
  typecheck [] expr; exp_to_value (eval' [] expr)

let eval_print (expr: exp) : unit =
  let rec eval' (env: (int * value) list) (e: exp) : exp =
    if is_value e then e
    else
      (let env', result = (step env e) in
      print_endline ("--> " ^ (string_of_exp result));
      (eval' env' result)) in
  typecheck [] expr;
  print_endline ("    " ^ string_of_exp expr);
  eval' [] expr; ()
