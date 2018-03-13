%{
  open Lang
%}

%token <int> INT
%token <bool> BOOL
%token <string> NAME

%token LPAREN     (* ( *)
%token RPAREN     (* ) *)
%token PLUS       (* + *)
%token MINUS      (* - *)
%token TIMES      (* * *)
%token DIVIDE     (* / *)
%token LEQ        (* <= *)
%token GEQ        (* >= *)
%token LSTHN      (* < *)
%token GTTHN      (* > *)
%token IF         (* if *)
%token THEN       (* then *)
%token ELSE       (* else *)
%token LET        (* let *)
%token EQUALS     (* = *)
%token IN         (* in *)
%token FUN        (* fun *)
%token ARROW      (* -> *)
%token FIX        (* fix *)
%token COLON      (* : *)
%token TINT       (* int *)
%token TBOOL      (* bool *)

%token EOF

%start <Lang.exp> prog

%%

prog:
  | e=exp EOF  { e }

(* Look back at function application *)

exp:
  | e1=base_exp op=bin_op e2=exp                     { EBinOp (op, e1, e2) }
  | IF e1=exp THEN e2=exp ELSE e3=exp                { EIf (e1, e2, e3) }
  | LET n=NAME COLON t=typ EQUALS e1=exp IN e2=exp   { ELet (EVar n, e1, e2, t) }
  | f=base_exp e=exp                                 { EFunCall (f, e) }
  | e=base_exp                                       { e }

bin_op:
  | PLUS   { OAdd }
  | MINUS  { OSubtract }
  | TIMES  { OMultiply }
  | DIVIDE { ODivide }
  | LEQ    { OLessThanEq }
  | GEQ    { OGreaterThanEq }
  | LSTHN  { OLessThan }
  | GTTHN  { OGreaterThan }
  | EQUALS { OEquals }

typ:
  | TINT                { TInt }
  | TBOOL               { TBool }
  | t1=typ ARROW t2=typ { TFun(t1, t2) }
  | LPAREN t=typ RPAREN { t }

base_exp:
  | FUN LPAREN n=NAME COLON t1=typ RPAREN
    COLON t2=typ ARROW e=exp              { EVal (VFun (EVar n, e, t1, t2)) }
  | FIX n1=NAME LPAREN n2=NAME COLON t1=typ RPAREN
    COLON t2=typ ARROW e=exp              { EVal (VFix (EVar n1, EVar n2, e, t1, t2)) }
  | i=INT                                 { EVal (VLit (LInt i)) }
  | b=BOOL                                { EVal (VLit (LBool b)) }
  | n=NAME                                { EVar n }
  | LPAREN e=exp RPAREN                   { e }
