%%

%name PlcParser

%pos int

%term VAR | FUN | FUNREC
    | COMMA | SEMIC | COLON | DCOLON
    | IF | THEN | ELSE
    | MATCH | WITH
    | HEAD | TAIL | ISE | PRINT
    | EQ | NEQ | EXCL | AND | LT | LTE | GTE
    | PLUS | MINUS | MULTI | DIV
    | RBRA | LBRA
    | RKEY | LKEY
    | RPAR | LPAR
    | FN | END
    | TRUE | FALSE
    | PIPE | ARROW | USCORE
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
    | Cond_expr of expr
    | Typed_var of plcVal
    | Params of plcType
    | Types of ListT

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
    | EXCL Expr (Prim1("!", Expr))
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