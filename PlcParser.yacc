%%

%name PlcParser

%pos int

%term VAR | FUN | FUNREC
    | COMMA | SEMIC | COLON | DCOLON
    | IF | THEN | ELSE
    | MATCH | WITH
    | HEAD | TAIL | ISE | PRINT
    | EQ | NEQ | NOT | AND | LT | LTE | GTE
    | PLUS | MINUS | MULTI | DIV
    | RBRA | LBRA
    | RKEY | LKEY
    | RPAR | LPAR
    | FN | END
    | TRUE | FALSE
    | PIPE | ARROW | DARROW | USCORE
    | TNIL | TBOOL | TINT 
    | EOF

%nonterm Prog of expr
    | Expr of expr
    | Decl of expr
    | Name of string
    | Args of expr
    | Type of plcType
    | AtomicType of plcType
    | AtomicExpr of expr 
    | AppExpr of expr 
    | MatchExpr of expr
    | Nat of int
    | Const of expr 
    | Comps of expr
    | CondExpr of expr
    | TypedVar of plcVal
    | Params of plcType
    | Types of ListT

%left ELSE AND EQ NEQ LT LTE PLUS MINUS MULTI DIV RBRA
%right SEMIC ARROW DCOLON
%nonassoc IF NOT HEAD TAIL ISE PRINT 

%eop EOF

%noshift EOF

%start Prog

%%
Prog : Expr (Expr)
    | Decl (Decl)

Decl : VAR Name EQ Expr SEMIC Prog (Let(Name, Expr, Prog))
    | FUN Name Args EQ Expr ()
    | FUNREC Name Args COLON Type EQ Expr ()

Expr : AtomicExpr (AtomicExpr)
    | AppExpr (AppExpr)
    | IF Expr THEN Expr ELSE Expr (If(Expr1, Expr2, Expr3))
    | MATCH Expr WITH MatchExpr ()
    | NOT Expr (Prim1("!", Expr))
    | MINUS Expr (Prim1("-", Expr))
    | HEAD Expr ()
    | TAIL Expr ()
    | ISE Expr ()
    | PRINT Expr ()
    | Expr AND Expr (Prim2("&&", Expr1, Expr2))
    | Expr PLUS Expr (Prim2("+", Expr2, Expr2))
    | Expr MINUS Expr (Prim2("-", Expr2, Expr2))
    | Expr MULTI Expr (Prim2("*", Expr2, Expr2))
    | Expr DIV Expr (Prim2("/", Expr2, Expr2))
    | Expr EQ Expr (Prim2("=", Expr2, Expr2))
    | Expr NEQ Expr (Prim2("!=", Expr2, Expr2))
    | Expr LT Expr (Prim2("<", Expr2, Expr2))
    | Expr LTE Expr (Prim2("<=", Expr2, Expr2))
    | Expr DCOLON Expr (Prim2("::", Expr1, Expr2))
    | Expr SEMIC Expr (Prim2(";", Expr1, Expr2))
    | Expr RBRA Nat LBRA ()

AtomicExpr : Const (Const)
    | Name (Name)
    | RBRA Prog LBRA ()
    | RPAR Expr LPAR (Expr)
    | RPAR Comps LPAR (Comps)
    | FN Args DARROW Expr END ()
(*
AppExpr : AtomicExpr AtomicExpr ()
    | AppExpr AtomicExpr ()
*)
Const : TRUE (ConB(TRUE))
    | FALSE (ConB(FALSE))
    | Nat (ConI(Nat))
    | LPAR RPAR ()
    | LPAR Type LBRA RBRA RPAR ()

Comps : Expr COMMA Expr ()
    | Expr COMMA Comps ()

MatchExpr : END ()
    | PIPE CondExpr ARROW Expr MatchExpr ()

CondExpr : Expr (Expr)
    | USCORE ()

Args : RPAR LPAR ()
    | RPAR Params LPAR ()

Params : TypedVar (TypedVar)
    | TypedVar COMMA Params ()

TypedVar : Type Name ()

Type : AtomicType (AtomicType)
    | LPAR Types RPAR ()
    | LBRA Type RBRA ()
    | Type ARROW Type ()

AtomicType : TNIL ()
    | TBOOL (BoolT)
    | TINT (IntT)
    | LPAR Type RPAR ()

Types : Type COMMA Type ()
    | Type COMMA Types ()