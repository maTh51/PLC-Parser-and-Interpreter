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
exception ListOutOfRange (*só pra lista, sequência não, porque Item é só pra lista (regra 25) *)
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
    | teval (Call(e2, e1)) (env:plcType env) = (*11*)
        let
            val e2Type = teval e2 env handle SymbolNotFound => let in raise NotFunc end
            val e1Type = teval e1 env
        in
            case e2Type of
                FunT (t1, t2) => if t1 = e1Type then t1 else raise CallTypeMisM
              | _ => raise UnknownType
        end
    | teval (If(e, e1, e2)) (env:plcType env) = (*12*)
        let
            val cond = teval e env
            val t1 = teval e1 env
            val t2 = teval e2 env
        in
            if cond = BoolT
                then if t1 = t2
                        then t1
                        else raise NotEqTypes
                else raise IfCondNotBool
        end
    | teval (Match(e, matches)) (env:plcType env) = (*13*)
        if matches = []
        then raise NoMatchResults
        else
            let
                val eType = teval e env
                val r1Type = teval (#2 (hd matches)) env

                fun checkMatch (h::[]) = 
                    let in
                        case h of
                              (SOME m, r) => if teval m env = eType
                                               then if teval r env = r1Type
                                                        then r1Type
                                                        else raise MatchResTypeDiff
                                               else raise MatchCondTypesDiff
                            | (NONE, r) => if teval r env = r1Type
                                            then r1Type
                                            else raise MatchResTypeDiff
                    end
                  | checkMatch (h::t) =
                    let
                        val ht = checkMatch(h::[])
                    in
                        checkMatch(t)
                    end
            in
                checkMatch(matches)
            end
    | teval (Prim1("!", e)) (env:plcType env) = (*14*)
        if teval e env = BoolT then BoolT else raise UnknownType
    | teval (Prim1("-", e)) (env:plcType env) = (*15*)
        if teval e env = IntT then IntT else raise UnknownType
    | teval (Prim2("&&", e1, e2)) (env:plcType env) = (*20*)
        if teval e1 env = BoolT andalso teval e2 env = BoolT
            then BoolT
            else raise UnknownType
		| teval (Prim2("::", e1, e2)) (env:plcType env) = (*21*)
				let 
					val tevalE1 = teval e1 env
					val tevalE2 = teval e2 env
				in
            case (tevalE1, tevalE2) of
                (IntT, ListT []) => SeqT IntT
              | (IntT, SeqT s) => if s = IntT then SeqT s else raise NotEqTypes
              | (BoolT, ListT []) => SeqT BoolT
              | (BoolT, SeqT s) => if s = BoolT then SeqT s else raise NotEqTypes
              | (ListT l, ListT []) => SeqT (ListT l)
              | (ListT l, SeqT s) => if s = ListT l then SeqT s else raise NotEqTypes
              | _ => raise UnknownType
        end
		| teval (Prim2("+", e1, e2)) (env:plcType env) = (*22*)
				if teval e1 env = IntT andalso teval e2 env = IntT then IntT else raise UnknownType
		| teval (Prim2("-", e1, e2)) (env:plcType env) = (*22*)
				if teval e1 env = IntT andalso teval e2 env = IntT then IntT else raise UnknownType
		| teval (Prim2("*", e1, e2)) (env:plcType env) = (*22*)
				if teval e1 env = IntT andalso teval e2 env = IntT then IntT else raise UnknownType
		| teval (Prim2("/", e1, e2)) (env:plcType env) = (*22*)
				if teval e1 env = IntT andalso teval e2 env = IntT then IntT else raise UnknownType

		| teval (Prim2("<", e1, e2)) (env:plcType env) = (*23*)
				if teval e1 env = IntT andalso teval e2 env = IntT then BoolT else raise UnknownType
		| teval (Prim2("<=", e1, e2)) (env:plcType env) = (*23*)
				if teval e1 env = IntT andalso teval e2 env = IntT then BoolT else raise UnknownType

		| teval (Prim2("=", e1, e2)) (env:plcType env) = (*24*)
				if teval e1 env = teval e2 env andalso (teval e1 env = IntT orelse teval e1 env = BoolT)
				then BoolT 
				else raise UnknownType
		| teval (Prim2("!=", e1, e2)) (env:plcType env) = (*24*)
				if teval e1 env = teval e2 env andalso (teval e1 env = IntT orelse teval e1 env = BoolT)
				then BoolT 
				else raise UnknownType
		
		| teval (Item(it, e)) (env:plcType env) = (*25*)
				let
						fun getIthElem (ith, []) = raise ListOutOfRange
							|	getIthElem (ith, (h::[])) = if ith = 1 then h else raise ListOutOfRange
							| getIthElem (ith, (h::t)) = if ith = 1 then h else getIthElem (ith - 1, t)
				in
						case (teval e env) of
								ListT l => getIthElem(it, l)
								| _ => raise OpNonList
				end
		
		| teval (Prim2(";", e1, e2)) (env:plcType env) = (*26*)
				let
						val t1 = teval e1 env
				in
						teval e2 env
				end