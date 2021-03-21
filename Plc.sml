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
use "PlcChecker.sml";
use "PlcInterp.sml";
use "Parse.sml";
open PlcFrontEnd;

fun run (e:expr) : string =
    val2string (eval e [])
    ^ " : " ^
    type2string (teval e []);