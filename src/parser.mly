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

%token EOF

%start <Lang.exp> prog

%%

prog:
  | e=exp EOF                             { e }

exp:
  | i=INT                                 { ELit (LInt i) }
  | b=BOOL                                { ELit (LBool b) }
  | LPAREN op=bin_op e1=exp e2=exp RPAREN { EBinOp (op, e1, e2) }
  | LPAREN IF e1=exp e2=exp e3=exp RPAREN { EIf (e1, e2, e3) }

bin_op:
  | PLUS   { OAdd }
  | MINUS  { OSubtract }
  | TIMES  { OMultiply }
  | DIVIDE { ODivide }
  | LEQ    { OLessThanEq }
