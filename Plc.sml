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
        handle SymbolNotFound => let val p = print ("type error: SymbolNotFound") in raise SymbolNotFound end
            | EmptySeq => let val p =  print ("type error: EmptySeq") in raise EmptySeq end
            | UnknownType => let val p =  print ("type error: ") in raise UnknownType end
            | NotEqTypes => let val p =  print ("type error: ") in raise NotEqTypes end
            | WrongRetType => let val p =  print ("type error: WrongRetType") in raise WrongRetType end
            | DiffBrTypes => let val p =  print ("type error: ") in raise DiffBrTypes end
            | IfCondNotBool => let val p =  print ("type error: ") in raise IfCondNotBool end
            | NoMatchResults => let val p =  print ("type error: ") in raise NoMatchResults end
            | MatchResTypeDiff => let val p =  print ("type error: ") in raise MatchResTypeDiff end
            | MatchCondTypesDiff => let val p =  print ("type error: ") in raise MatchCondTypesDiff end
            | CallTypeMisM => let val p =  print ("type error: ") in raise CallTypeMisM end
            | NotFunc => let val p =  print ("type error: ") in raise NotFunc end
            | ListOutOfRange => let val p =  print ("type error: ") in raise ListOutOfRange end
            | OpNonList  => let val p =  print ("type error: ") in raise OpNonList end
            | _ => let val p = "type error: UnknownType" in raise UnknownType end

        val expResult = 
            let in
                eval exp []
            end
        handle SymbolNotFound => let val p = print ("eval error: SymbolNotFound") in raise SymbolNotFound end
            | ValueNotFoundInMatch => let val p = print ("eval error: Match expression was not found among the options!") in raise ValueNotFoundInMatch end
            | _ => let val p = print ("eval error: Impossible") in raise Impossible end
    in
        val2string(expResult) ^ " : " ^ type2string(expType)
    end
