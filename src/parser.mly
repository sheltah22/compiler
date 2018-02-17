%{
  open Lang
%}

%token <int> INT
%token <bool> BOOL

%token LPAREN     (* ( *)
%token RPAREN     (* ) *)
%token PLUS       (* + *)
%token MINUS      (* - *)
%token TIMES      (* * *)
%token DIVIDE     (* / *)
%token LEQ        (* <= *)
%token IF         (* if *)
%token THEN       (* then *)
%token ELSE       (* else *)

%token EOF

%start <Lang.exp> prog

%%

prog:
  | e=exp EOF  { e }

exp:
  | e1=base_exp op=bin_op e2=exp       { EBinOp (op, e1, e2) }
  | IF e1=exp THEN e2=exp ELSE e3=exp  { EIf (e1, e2, e3) }
  | e=base_exp                         { e }

bin_op:
  | PLUS   { OAdd }
  | MINUS  { OSubtract }
  | TIMES  { OMultiply }
  | DIVIDE { ODivide }
  | LEQ    { OLessThanEq }

base_exp:
  | i=INT               { ELit (LInt i) }
  | b=BOOL              { ELit (LBool b) }
  | LPAREN e=exp RPAREN {e}
