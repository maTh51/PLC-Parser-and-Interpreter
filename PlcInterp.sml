(* PlcInterp *)

exception Impossible
exception HDEmptySeq
exception TLEmptySeq
exception ValueNotFoundInMatch
exception NotAFunc

(*
    variável livre, tenta procurar no ambiente e não encontra
    aí vai ter a exceção symbolnotfoud (exceção do environ)
    fazer testes tbm passando um environ, não só com ele vazio
*)

fun eval (Var v) (env:plcVal env) = (*1*)
        let in 
            lookup env v 
        handle 
            SymbolNotFound => raise SymbolNotFound 
        end
	| eval (ConI n) (_) = IntV n (*2*)
	| eval (ConB b) (_) = BoolV b (*3, 4*)
	| eval (List []) (_) = ListV [] (*5*)
	| eval (List l) (env:plcVal env) = (*6*)
		let 
			fun evalList (h::[]) = eval h env :: []
				| evalList (h::t) = eval h env :: evalList t
				| evalList (_) = raise Impossible;
		in
			ListV (evalList l)
		end
	| eval (ESeq t) (_) = SeqV [] (*7*)
	| eval (Let(var, t1, t2)) (env:plcVal env) = (*8*)
		let
            val evalT1 = eval t1 env
            val mapEnv = (var, evalT1) :: env
        in
            eval t2 mapEnv
        end
	| eval (Letrec(nameFun, typeArg, arg, typeFun, t1, t2)) (env:plcVal env) = (*9*)
		let
			val mapEnv = (nameFun, Clos(nameFun, arg, t1, env)) :: env
		in
			eval t2 mapEnv
		end
    | eval (Anon(s, x, e)) (env:plcVal env) = Clos("", x, e, env) (*10*)