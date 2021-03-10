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
    | TYPENIL | TYPEBOOL | TYPEINT 
    | EOF

%nonterm Prog of expr
    | Expr of expr
    | Decl of expr
    | Name of string
    | Args of expr
    | Type of plcType
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
    | FUN Name Args EQ Expr (lex())
    | FUNREC Name Args COLON Type EQ Expr (lex())

Expr : AtomicExpr (AtomicExpr)
    | AppExpr (AppExpr)
    | IF Expr THEN Expr ELSE Expr (If(Expr1, Expr2, Expr3))
    | MATCH Expr WITH MatchExpr (lex())
    | NOT Expr (Prim1("!", Expr))
    | MINUS Expr (Prim1("-", Expr))
    | HEAD Expr (lex())
    | TAIL Expr (lex())
    | ISE Expr (lex())
    | PRINT Expr (lex())
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
    | Expr RBRA Nat LBRA (lex())

AtomicExpr : Const (Const)
    | Name (Name)
    | RBRA Prog LBRA (lex())
    | RPAR Expr LPAR (Expr)
    | RPAR Comps LPAR (Comps)
    | FN Args DARROW Expr END (lex())

AppExpr : AtomicExpr AtomicExpr (lex())
    | AppExpr AtomicExpr (lex())

Const : TRUE (ConB(TRUE))
    | FALSE (ConB(FALSE))
    | Nat (ConI(Nat))
    | LPAR RPAR (lex())
    | LPAR Type LBRA RBRA RPAR (lex())

Comps : Expr COMMA Expr (lex())
    | Expr COMMA Comps (lex())

MatchExpr : END (lex())
    | PIPE CondExpr ARROW Expr MatchExpr (lex())

CondExpr : Expr (Expr)
    | USCORE (lex())

Args : RPAR LPAR (lex())
    | RPAR Params LPAR (lex())

Params : TypedVar (TypedVar)
    | TypedVar COMMA Params (lex())

    