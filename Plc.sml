(* Plc interpreter main file *)

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
        | WrongRetType => let val p =  print ("type error: WrongRetType") in raise WrongRetType end
        | _ => let val p = print ("type error: UnknownType") in raise UnknownType end
    val expResult = 
      let in
        eval exp []
      end
      handle SymbolNotFound => let val p = print ("eval error: SymbolNotFound") in raise SymbolNotFound end
        | _ => let val p = print ("eval error: Impossible") in raise Impossible end
  in
    val2string(expResult) ^ " : " ^ type2string(expType)
  end