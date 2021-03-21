(* PlcChecker *)


exception EmptySeq
exception UnknownType
exception NotEqTypes (*pra regra 21, por exemplo, que precisa de e1 ter o mesmo tipo de e2*)
exception WrongRetType
exception DiffBrTypes (*avaliando if (cond, expr1, expr2). se a expr1 tiver tipo diferente de expr2, dispara essa. different branch types*)
exception IfCondNotBool
exception NoMatchResults
exception MatchResTypeDiff
exception MatchCondTypesDiff
exception CallTypeMisM
exception NotFunc
exception ListOutOfRange (*só pra lista, sequência não, parece que nao tem como fazer seleção de item em sequência. Item é só pra lista, pra sequência não. Item tem que ser lista (regra 25) *)
exception OpNonList (*quando for avaliar um Item. se a expressão em que tiver tentando usar não for uma lista, dispara essa exceção*)

(*
    não vai ter um case pra cada regra

*)

fun teval (e:expr) (p:plcType env) : plcType= 
    case e of
        ConI _ => IntT (*2*)
        | ConB _ => BoolT (*3, 4*)
        | List [] => ListT [] (*5*)
        | ESeq t => t (*7*)
        | Let(n, e, pr) => (*8*)
            let
                val t1 = teval e p
                val t2 = teval pr p
            in
                t2
            end
        | Prim1("!", e) => if teval e p = BoolT then BoolT else raise UnknownType (*14*)
        | Prim1("-", e) => if teval e p = IntT then IntT else raise UnknownType (*15*)
(*
        | Prim1("hd", Expr) => (*16*)
            let
                val st = teval Expr p
            in
                case st of
                    SeqT t => t
                    | _ => raise UnknownType
            end
        | Prim1("tl", Expr) => if  then  else 
        | Prim1("ise", Expr) => if  then  else 
        | Prim1("print", Expr) => if  then  else 
        | Prim2("&&", Expr1, Expr2) => if  then  else 
        | Prim2("+", Expr1, Expr2) => if  then  else 
        | Prim2("-", Expr1, Expr2) => if  then  else 
        | Prim2("*", Expr1, Expr2) => if  then  else 
        | Prim2("/", Expr1, Expr2) => if  then  else 
        | Prim2("=", Expr1, Expr2) => if  then  else 
        | Prim2("!=", Expr1, Expr2) => if  then  else 
        | Prim2("<", Expr1, Expr2) => if  then  else 
        | Prim2("<=", Expr1, Expr2) => if  then  else 
        | Prim2("::", Expr1, Expr2) => if  then  else 
        | Prim2(";", Expr1, Expr2) => if  then  else
        *)
        | _ => raise UnknownType;