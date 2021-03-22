(* Plc interpreter main file *)

(*
    o handle fica com um let que define duas variáveis
    com a exceção do teval e do eval
    e aí o handle vê esse par
*)

CM.make("$/basis.cm");
CM.make("$/ml-yacc-lib.cm");

use "Environ.sml";
use "Absyn.sml";
use "PlcParserAux.sml";
use "PlcParser.yacc.sig";
use "PlcParser.yacc.sml";
use "PlcLexer.lex.sml";

use "Parse.sml";
use "PlcInterp.sml";
use "PlcChecker.sml";

Control.Print.printLength := 2028;
Control.Print.printDepth  := 2028;
Control.Print.stringDepth := 2028;

open PlcFrontEnd;

fun run exp =
    let
        val expType = 
            let in
                teval exp []
            end
			handle SymbolNotFound => let val p = print ("type error: Attempt to use undefined variable.") in raise SymbolNotFound end
				| EmptySeq => let val p =  print ("type error: Unable to evaluate empty sequence.") in raise EmptySeq end
				| UnknownType => let val p =  print ("type error: Unknown type.") in raise UnknownType end
				| NotEqTypes => let val p =  print ("type error: Operator :: received expressions with different types, but they should be the same.") in raise NotEqTypes end
				| WrongRetType => let val p =  print ("type error: Recursive function expected different type for the return value.") in raise WrongRetType end
				| DiffBrTypes => let val p =  print ("type error: If statement with different branch types, but they should be the same.") in raise DiffBrTypes end
				| IfCondNotBool => let val p =  print ("type error: If statement with a condition that is not boolean.") in raise IfCondNotBool end
				| NoMatchResults => let val p =  print ("type error: Match statement doesn't have any match cases.") in raise NoMatchResults end
				| MatchResTypeDiff => let val p =  print ("type error: Match statement with possible results of different types, when they should be the same.") in raise MatchResTypeDiff end
				| MatchCondTypesDiff => let val p =  print ("type error: Match statement is unable to match expressions of different types.") in raise MatchCondTypesDiff end
				| CallTypeMisM => let val p =  print ("type error: Function parameter expected expression of a different type.") in raise CallTypeMisM end
				| NotFunc => let val p =  print ("type error: Attempt to call undefined function.") in raise NotFunc end
				| ListOutOfRange => let val p =  print ("type error: Item index out of list's range.") in raise ListOutOfRange end
				| OpNonList  => let val p =  print ("type error: Attempt to use Item operator with a non-list type.") in raise OpNonList end
				| _ => let val p = "type error: Unknown type." in raise UnknownType end

        val expResult = 
            let in
                eval exp []
            end
			handle SymbolNotFound => let val p = print ("eval error: Attempt to evaluate undefined variable.") in raise SymbolNotFound end
				| Impossible => let val p = print ("eval error: Impossible evaluation.") in raise Impossible end
				| HDEmptySeq => let val p = print ("eval error: Attempt to use operator hd with empty sequence.") in raise HDEmptySeq end
				| TLEmptySeq => let val p = print ("eval error: Attempt to use operator tl with empty sequence.") in raise TLEmptySeq end
				| ValueNotFoundInMatch => let val p = print ("eval error: Expression did not match any of the options.") in raise ValueNotFoundInMatch end
				| NotAFunc => let val p = print ("eval error: Attempt to call undefined function.") in raise NotAFunc end
				| _ => let val p = print ("eval error: Impossible evaluation.") in raise Impossible end
    in
        val2string(expResult) ^ " : " ^ type2string(expType)
    end
