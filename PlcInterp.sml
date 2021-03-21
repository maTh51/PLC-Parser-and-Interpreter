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

fun eval (e:expr) (p:plcType env) : plcVal =
    case e of
          ConI x => IntV x
        | ConB x => BoolV x
        | _ => raise Impossible;
    