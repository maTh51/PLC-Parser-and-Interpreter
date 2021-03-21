(* PlcChecker *)

exception EmptySeq
exception UnknownType
exception NotEqTypes
exception WrongRetType
exception DiffBrTypes
exception IfCondNotBool
exception NoMatchResults
exception MatchResTypeDiff
exception MatchCondTypesDiff
exception CallTypeMisM
exception NotFunc
exception ListOutOfRange
exception OpNonList


fun teval (Var v) (env:plcType env) = 
	let in 
		lookup env v 
	handle 
		SymbolNotFound => raise SymbolNotFound 
	end
	| teval (ConI _) (_) = IntT
	| teval (ConB _) (_) = BoolT
	| teval (List l) (env:plcType env) = 
		let 
			fun tevalList (h::[]) = (teval h env)::[]
				| tevalList (h::t) = (teval h env)::(tevalList t)
				| tevalList (_) = []
		in
			ListT (tevalList l)
		end
	| teval (ESeq t) _ = 
		let in
			case t of
	  		SeqT elem => SeqT elem
				| _ => raise EmptySeq
		end 
	| teval (Let(var, t1, t2)) (env:plcType env) =
		let
      val tevalT1 = teval t1 env
      val mapEnv = (var, tevalT1) :: env
    in
      teval t2 mapEnv
    end
	| teval (Letrec(nameFun, typeArg, arg, typeFun, t1, t2)) (env:plcType env) =
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

	

