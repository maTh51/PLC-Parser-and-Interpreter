%%

%name PlcParser

%pos int

%term VAR | FUN | REC
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
    | NIL
    | Name of string
    | Nat of int
    | EOF

%nonterm Prog of expr
    | Expr of expr
    | Decl of expr
    | Args of ( plcType * string ) list
    | Type of plcType
    | Types of plcType list
    | AtomicType of plcType
    | AtomicExpr of expr 
    | AppExpr of expr 
    | MatchExpr of (expr option * expr) list
    | Comps of expr list
    | CondExpr of expr option
    | TypedVar of plcType * string
    | Params of ( plcType * string ) list
    | Const of expr

%right SEMIC ARROW
%nonassoc IF
%left ELSE
%left AND
%left EQ NEQ
%left LT LTE
%right DCOLON
%left PLUS MINUS 
%left MULTI DIV
%nonassoc NOT HEAD TAIL ISE PRINT Name
%left LBRA

%eop EOF

%noshift EOF

%start Prog

%%
Prog : Expr (Expr)
    | Decl (Decl)

Decl : VAR Name EQ Expr SEMIC Prog (Let(Name, Expr, Prog))
    | FUN Name Args EQ Expr SEMIC Prog (Let(Name, makeAnon(Args, Expr), Prog))
    | FUN REC Name Args COLON Type EQ Expr SEMIC Prog (makeFun(Name, Args, Type, Expr, Prog))

Expr : AtomicExpr (AtomicExpr)
    | AppExpr (AppExpr)
    | IF Expr THEN Expr ELSE Expr (If(Expr1, Expr2, Expr3))
    | MATCH Expr WITH MatchExpr (Match(Expr, MatchExpr))
    | NOT Expr (Prim1("!", Expr))
    | MINUS Expr (Prim1("-", Expr))
    | HEAD Expr (Prim1("hd", Expr))
    | TAIL Expr (Prim1("tl", Expr))
    | ISE Expr (Prim1("ise", Expr))
    | PRINT Expr (Prim1("print", Expr))
    | Expr AND Expr (Prim2("&&", Expr1, Expr2))
    | Expr PLUS Expr (Prim2("+", Expr1, Expr2))
    | Expr MINUS Expr (Prim2("-", Expr1, Expr2))
    | Expr MULTI Expr (Prim2("*", Expr1, Expr2))
    | Expr DIV Expr (Prim2("/", Expr1, Expr2))
    | Expr EQ Expr (Prim2("=", Expr1, Expr2))
    | Expr NEQ Expr (Prim2("!=", Expr1, Expr2))
    | Expr LT Expr (Prim2("<", Expr1, Expr2))
    | Expr LTE Expr (Prim2("<=", Expr1, Expr2))
    | Expr DCOLON Expr (Prim2("::", Expr1, Expr2))
    | Expr SEMIC Expr (Prim2(";", Expr1, Expr2))
    | Expr LBRA Nat RBRA (Item(Nat, Expr))

AtomicExpr : Const (Const)
    | Name (Var(Name))
    | LKEY Prog RKEY (Prog)
    | LPAR Expr RPAR (Expr)
    | LPAR Comps RPAR (List Comps)
    | FN Args DARROW Expr END (makeAnon(Args, Expr))

AppExpr : AtomicExpr AtomicExpr (Call(AtomicExpr1, AtomicExpr2))
    | AppExpr AtomicExpr (Call(AppExpr, AtomicExpr))

Const : TRUE (ConB(true))
    | FALSE (ConB(false))
    | Nat (ConI(Nat))
    | LPAR RPAR (List [])
    | LPAR Type LBRA RBRA RPAR (ESeq(Type))

Comps : Expr COMMA Expr ([Expr1, Expr2])
    | Expr COMMA Comps ([Expr] @ Comps)

MatchExpr : END ([])
    | PIPE CondExpr ARROW Expr MatchExpr ([(CondExpr, Expr)] @ MatchExpr)

CondExpr : Expr (SOME(Expr))
    | USCORE (NONE)

Args : LPAR RPAR ([])
    | LPAR Params RPAR (Params)

Params : TypedVar ([TypedVar])
    | TypedVar COMMA Params ([TypedVar] @ Params)

TypedVar : Type Name (Type, Name)

Type : AtomicType (AtomicType)
    | LPAR Types RPAR (ListT (Types))
    | LBRA Type RBRA (SeqT(Type))
    | Type ARROW Type (FunT(Type1, Type2))

AtomicType : TNIL (ListT [])
    | TBOOL (BoolT)
    | TINT (IntT)
    | LPAR Type RPAR (Type)

Types : Type COMMA Type ([Type1, Type2])
    | Type COMMA Types ([Type] @ Types)