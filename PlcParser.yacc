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
    | Match_expr of expr
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
    