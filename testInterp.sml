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
teval (fromString "var x = 9; x+3") [];
eval (fromString "var x = 9; x+3") [];
eval (fromString "fun f(Int x) = -x; f(5)") [];

