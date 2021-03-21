(* PlcInterp *)

exception Impossible
exception HDEmptySeq
exception TLEmptySeq
exception ValueNotFoundInMatch
exception NotAFunc


fun eval (Var v) (env:plcVal env) = 
	let in 
		lookup env v 
	handle 
		SymbolNotFound => raise SymbolNotFound 
	end
	| eval (ConI n) (_) = IntV n
	| eval (ConB b) (_) = BoolV b 
	| eval (List []) (_) = ListV []
	| eval (List l) (env:plcVal env) = 
		let 
			fun evalList (h::[]) = eval h env :: []
				| evalList (h::t) = eval h env :: evalList t
				| evalList (_) = raise Impossible;
		in
			ListV (evalList l)
		end
	| eval (ESeq t) (_) = SeqV []
	| eval (Let(var, t1, t2)) (env:plcVal env) =
		let
      val evalT1 = eval t1 env
      val mapEnv = (var, evalT1) :: env
    in
      eval t2 mapEnv
    end
	| eval (Letrec(nameFun, typeArg, arg, typeFun, t1, t2)) (env:plcVal env) =
		let
			val mapEnv = (nameFun, Clos(nameFun, arg, t1, env)) :: env
		in
			eval t2 mapEnv
		end
	
		