use "Plc.sml";
use "PlcChecker.sml";

teval (fromString "15") [];
eval (fromString "15") [];
teval (fromString "true") [];
eval (fromString "true") [];
teval (fromString "()") [];
eval (fromString "()") [];
teval (fromString "(1,2,3)") [];
eval (fromString "(1,2,3)") [];
teval (fromString "([Bool] [])") [];
eval (fromString "([Bool] [])") [];
teval (fromString "var x = 9; x") [];
eval (fromString "var x = 9; true") [];
(* eval (fromString "fun rec f (Int n) : Int = n; f(5)") []; *)

