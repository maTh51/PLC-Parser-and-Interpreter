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
    | Args of ( plcType * string ) list
    | Type of plcType
    | AtomicType of plcType
    | AtomicExpr of expr 
    | AppExpr of expr 
    | MatchExpr of expr
    | Nat of int
    | Const of expr 
    | Comps of expr
    | CondExpr of expr
    | TypedVar of plcType * string
    | Params of (plcType * string ) list
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
    | FUN Name Args EQ Expr SEMIC Prog (Let(Name, makeAnon(Args, Expr), Prog))
    | FUNREC Name Args COLON Type EQ Expr SEMIC Prog (makeFun(Name, Args, Type, Expr, Prog))

Expr : AtomicExpr (AtomicExpr)
    | AppExpr (AppExpr)
    | IF Expr THEN Expr ELSE Expr (If(Expr1, Expr2, Expr3))
    | MATCH Expr WITH MatchExpr ()
    | NOT Expr (Prim1("!", Expr))
    | MINUS Expr (Prim1("~", Expr))
    | HEAD Expr (Prim1("hd", Expr))
    | TAIL Expr (Prim1("tl", Expr))
    | ISE Expr (Prim1("null", Expr))
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
    | Expr LBRA Nat RBRA (Item(Nat, Expr))

Params : TypedVar (TypedVar::[])
    | TypedVar COMMA Params (TypedVar::Params)

TypedVar : Type Name (Type, Name)
