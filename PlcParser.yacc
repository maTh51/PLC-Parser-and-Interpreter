%%

%name PlcParser

%pos int

%term 

%nonterm prog of expr | decl of expr | atomic_expr of expr | app_expr

%eop EOF

%noshift EOF

%start Prog

%%
