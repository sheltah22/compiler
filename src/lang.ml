type binary_op =
  | OAdd
	| OSubtract
	| OMultiply
	| ODivide
	| OLessThanEq

type ternary_op =
  | OIf

type lit =
  | LInt of int
	| LBool of bool

type expr =
  | ELit of lit
  | EBinOp of binary_op * expr * expr
  | ETernOp of ternary_op * expr * expr * expr

type ast =
  | Root of expr

let typecheck_bin_op expr1 expr2 =
	match expr1, expr2 with
	| ELit (LInt _), ELit (LInt _) -> true
	| _, _ -> false

let typecheck_if expr1 expr2 expr3 =
	match (expr1, expr2, expr3) with
	| (ELit (LBool _), ELit (LInt _), ELit (LInt _)) -> true
	| (_, _, _) -> false

let rec interpret_expr (expr: expr) : expr =
  match expr with
  | ELit (LInt i) -> ELit (LInt i)
	| ELit (LBool b) -> ELit (LBool b)
  | EBinOp (op, expr1, expr2) ->
		begin
			let expr1 = interpret_expr expr1 in
			let expr2 = interpret_expr expr2 in
			let typechecks = typecheck_bin_op expr1 expr2 in
			if not typechecks
			  then failwith ("Error: typechecking failed, "
				              ^ "a binary operator should take 2 ints")
				else
					match op, expr1, expr2 with
		      | (OAdd, ELit (LInt i1), ELit (LInt i2)) -> ELit (LInt (i1 + i2))
					| (OSubtract, ELit (LInt i1), ELit (LInt i2)) -> ELit (LInt (i1 - i2))
					| (OMultiply, ELit (LInt i1), ELit (LInt i2)) -> ELit (LInt (i1 * i2))
					| (ODivide, ELit (LInt i1), ELit (LInt i2)) ->
						if i2 = 0 then failwith "Error: division by 0 attempted"
							else ELit (LInt (i1 / i2))
					| (OLessThanEq, ELit (LInt i1), ELit (LInt i2)) ->
						ELit (LBool (i1 <= i2))
		      | _ -> failwith ("Error: unrecognized operator")
    end
	| ETernOp (op, expr1, expr2, expr3) ->
		begin
			let expr1 = interpret_expr expr1 in
			let expr2 = interpret_expr expr2 in
			let expr3 = interpret_expr expr3 in
			let typechecks = typecheck_if expr1 expr2 expr3 in
			if not typechecks 
			  then failwith ("Error: typechecking failed, if function should take "
				              ^ "a boolean and two integers")
				else
					match op with
					| OIf -> begin
									   match expr1 with
									   | ELit (LBool b) -> if b then expr2 else expr3
									 end
		end

let interpret_tree (syntax_tree: ast) : expr =
  match syntax_tree with
  | Root (expr_init) -> interpret_expr expr_init

let returns_int expr =
	match expr with
	| ELit (LInt i) -> true
	| _ -> false

let return_int expr =
	match expr with
	| ELit (LInt i) -> i
	| _ -> failwith "Error: expr should evaluate to a int literal"

let return_bool expr =
	match expr with
	| ELit (LBool b) -> b
	| _ -> failwith "Error: expr should evaluate to a bool literal"

let print_expr expr =
	if returns_int expr then (print_endline (string_of_int (return_int expr)))
	  else (print_endline (string_of_bool (return_bool expr)))
