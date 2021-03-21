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
    | eval (Call (e1, e2)) (env:plcVal env) = (*11*)
				let
						fun getArgs (List (h::[])) = [eval h env]
							| getArgs (List (h::t)) = [eval h env] @ getArgs (List t)
							| getArgs (e) = [eval e env]
						
						val mapEnv = [("$list", ListV (getArgs e2))] @ env
						val evalE1 = eval e1 env
				in
						case evalE1 of Clos(name, var, e, auxEnv) =>
									let
										val evalE2 = eval e2 mapEnv
										val newEnv = (var, evalE2)::(name, evalE1)::auxEnv
									in
										eval e newEnv
									end
							| _ => raise NotAFunc
				end
    | eval (If(e, e1, e2)) (env:plcVal env) = (*12*)
        let in
            case (eval e env) of
                  BoolV true => eval e1 env
                | BoolV false => eval e2 env
                | _ => raise Impossible
        end
    | eval (Match(e, matches)) (env:plcVal env) = (*13*)
        let
            fun makeMatch (h::[]) =
                let in
                    case h of
                          (SOME m, r) => if eval e env = eval m env then eval r env else raise ValueNotFoundInMatch
                        | (NONE, r) => eval r env
                end
              | makeMatch (h::t) = 
                let in
                    case h of
                          (SOME m, r) => if eval e env = eval m env then eval r env else makeMatch(t)
                        | (NONE, r) => eval r env
                end
        in
            makeMatch(matches)
        end
    | eval (Prim1("!", e)) (env:plcVal env) = (*14*)
        let in
            case (eval e env) of 
                BoolV b => BoolV(not b)
                | _ => raise Impossible
        end
    | eval (Prim1("-", e)) (env:plcVal env) = (*15*)
        let in
            case (eval e env) of
                IntV i => IntV(~i)
                | _ => raise Impossible
        end
    | eval (Prim2("&&", e1, e2)) (env:plcVal env) = (*20*)
        let in
            case ((eval e1 env), (eval e2 env)) of
                (BoolV b1, BoolV b2) => BoolV(b1 andalso b2)
                | _ => raise Impossible
        end
		| eval (Prim2("::", e1, e2)) (env:plcVal env) = (*21*)
        let in
            case ((eval e1 env), (eval e2 env)) of
                (IntV i, SeqV s) => SeqV (IntV i :: s)
								| (BoolV b, SeqV s) =>  SeqV (BoolV b :: s)
								| (ListV l, SeqV s) => SeqV (ListV l :: s)
                | _ => raise Impossible
        end
		
		| eval (Prim2("+", e1, e2)) (env:plcVal env) = (*22*)
						let in
								case ((eval e1 env), (eval e2 env)) of (IntV i1, IntV i2) => IntV(i1 + i2)
								| _ => raise Impossible
						end
		| eval (Prim2("-", e1, e2)) (env:plcVal env) = (*22*)
						let in
								case ((eval e1 env), (eval e2 env)) of (IntV i1, IntV i2) => IntV(i1 - i2)
								| _ => raise Impossible
						end
		| eval (Prim2("*", e1, e2)) (env:plcVal env) = (*22*)
						let in
								case ((eval e1 env), (eval e2 env)) of (IntV i1, IntV i2) => IntV(i1 * i2)
								| _ => raise Impossible
						end
		| eval (Prim2("/", e1, e2)) (env:plcVal env) = (*22*)
						let in
								case ((eval e1 env), (eval e2 env)) of (IntV i1, IntV i2) => IntV(i1 div i2)
								| _ => raise Impossible
						end
		| eval (Prim2("<", e1, e2)) (env:plcVal env) = (*23*)
						let in
								case ((eval e1 env), (eval e2 env)) of (IntV i1, IntV i2) => BoolV(i1 < i2)
								| _ => raise Impossible
						end
		| eval (Prim2("<=", e1, e2)) (env:plcVal env) = (*23*)
						let in
								case ((eval e1 env), (eval e2 env)) of (IntV i1, IntV i2) => BoolV(i1 <= i2)
								| _ => raise Impossible
						end
		| eval (Prim2("=", e1, e2)) (env:plcVal env) = (*24*)
						let in
								case ((eval e1 env), (eval e2 env)) of (IntV i1, IntV i2) => BoolV(i1 = i2)
								| _ => raise Impossible
						end
		| eval (Prim2("!=", e1, e2)) (env:plcVal env) = (*24*)
						let in
								case ((eval e1 env), (eval e2 env)) of (IntV i1, IntV i2) => BoolV(i1 <> i2)
								| _ => raise Impossible
						end
        | eval (Prim2(";", e1, e2)) (env:plcVal env) = (*26*)
            let
                val v1 = eval e1 env
            in
                eval e2 env
            end