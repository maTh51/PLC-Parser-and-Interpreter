(* Infrastructure to run the Plc Front-End *)

CM.make("$/basis.cm");
CM.make("$/ml-yacc-lib.cm");

use "Environ.sml";
use "Absyn.sml";
use "PlcParserAux.sml";
use "PlcParser.yacc.sig";
use "PlcParser.yacc.sml";
use "PlcLexer.lex.sml";

use "Parse.sml";

Control.Print.printLength := 1000;
Control.Print.printDepth  := 1000;
Control.Print.stringDepth := 1000;
open PlcFrontEnd;


fromString "15";
fromString "-3 < 4";
fromString "true";
fromString "()";
fromString "(6,false)[1]";
fromString "([Bool] [])";
fromString "print x";
fromString "print x; true";
fromString "3::7::t";
fromString "var x = 9; x + 3";
fromString "fn (Int x) => -x end";
fromString "fun f(Int x) = x; f(1)";
fromString "fun f(Int x) = x; f(f(f(1)))";
fromString "match x with | 0 -> 1 | _ -> -1 end";
fromString "if x <= 0 then 1 else x";
fromString "fun rec f (Int x) : Int = if x <= 0 then 1 else x + f(x-1); f(5)";
fromString "fun rec f (Int x) : Int = if x <= 0 then 1 else f(x-1); f(5)";
fromString "fun rec f(Int n) : Int = if n <= 0 then 0 else n + f(n-1); f(5)";

use "testParserCases.sml";

(*
    para testar, precisa de uma (string * expr) list, como a do arquivo "testParserCases.sml",
    para o primeiro parâmetro da função, em que a string é uma expressão em PLC
    e a expr é a expr esperada dela depois do parser.

    o segundo parâmetro indica até qual caso de teste é para fazer o parsing.
    a ideia é que vá, manualmente, incrementando, primeiro testa 1, depois 2, ...
    até achar um erro na string n, corrigir e poder ir para n+1

    ela retorna uma lista de tuplas, em que o primeiro elemento é o resultado do parser
    e o segundo é o que era esperado. então, se cada tupla tiver os dois elementos iguais, deu certo!
*)

fun test (xs:(string * expr) list, ate:int):(expr * expr) list =
    case xs of
            [] => []
        | x::y => if ate = 0
                    then []
                    else [(fromString (#1 x), (#2 x))] @ test(y, ate-1)
    
