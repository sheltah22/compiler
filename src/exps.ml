(* exps.ml *)
(* defines all types used in other files *)
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
  | TTuple of typ list
  | TList of typ
  | TRef of typ
  | TDynamic
  | TVar of string

type value =
  | VLit of lit
  | VFun of exp * exp * typ * typ
  | VFix of exp * exp * exp * typ * typ
  | VUnit
  | VTuple of exp list
  | VEmptyList of typ
  | VCons of exp * exp
  | VPtr of int
  | VInferFun of exp * exp
  | VInferFix of exp * exp * exp
  | VInferEmptyList
and exp =
  | EVal of value
  | EBinOp of bin_op * exp * exp
  | EIf of exp * exp * exp
  | EVar of string
  | ELet of exp * exp * exp * typ
  | EFunCall of exp * exp
  | EHead of exp
  | ETail of exp
  | EEmpty of exp
  | ERef of exp
  | EAssign of exp * exp
  | EBang of exp
  | ESequence of exp * exp
  | EWhile of exp * exp * exp
  | ENth of exp * exp
  | EInferLet of exp * exp * exp
