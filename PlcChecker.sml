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


fun teval (Var v) (env:plcType env) = (*1*)
	let in 
		lookup env v 
	handle 
		SymbolNotFound => raise SymbolNotFound 
	end
	| teval (ConI _) (_) = IntT (*2*)
	| teval (ConB _) (_) = BoolT (*3, 4*)
	| teval (List l) (env:plcType env) = (*5, 6*)
		let 
			fun tevalList (h::[]) = (teval h env)::[]
				| tevalList (h::t) = (teval h env)::(tevalList t)
				| tevalList (_) = []
		in
			ListT (tevalList l)
		end
	| teval (ESeq t) _ = (*7*)
		let in
			case t of
	  		SeqT elem => SeqT elem
				| _ => raise EmptySeq
		end 
	| teval (Let(var, t1, t2)) (env:plcType env) = (*8*)
		let
            val tevalT1 = teval t1 env
            val mapEnv = (var, tevalT1) :: env
        in
            teval t2 mapEnv
        end
	| teval (Letrec(nameFun, typeArg, arg, typeFun, t1, t2)) (env:plcType env) = (*9*)
		let
			val envArg = (arg, typeArg)
			val types = (typeArg, typeFun)
			val envRecFun = (nameFun, FunT types)
            val tevalT1 = teval t1 (envRecFun :: envArg :: env)
            val tevalT2 = teval t2 (envRecFun :: env)
        in
            if tevalT1 = typeFun 
			    then tevalT2 
			    else raise WrongRetType
        end
    | teval (Anon(typeArg, arg, e)) (env:plcType env) = (*10*)
        let
            val envArg = (arg, typeArg)
            val eType = teval e (envArg :: env)
            val funType = (typeArg, eType)
        in
            FunT funType
        end
    | teval (Prim1("!", e)) (env:plcType env) = (*14*)
        if teval e env = BoolT then BoolT else raise UnknownType
    | teval (Prim1("-", e)) (env:plcType env) = (*15*)
        if teval e env = IntT then IntT else raise UnknownType
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