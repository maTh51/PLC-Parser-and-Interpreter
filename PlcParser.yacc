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
    | Name of
    | Args of
    | Type of
    | Atomic_expr of expr 
    | App_expr of expr 
    | Match_expr of expr
    | Nat of 
    | Const of expr 
    | Comps of 
    | Cond_expr of expr
    | Typed_var of
    | Params of
    | Types of ListT

%eop EOF

%noshift EOF

%start prog

%%
expr : expr PLUS expr (Prim2("+", expr1, expr2))

const : CINT (ConI(CINT))