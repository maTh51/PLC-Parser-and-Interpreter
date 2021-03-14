functor PlcParserLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : PlcParser_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct

end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\025\000\002\000\024\000\003\000\023\000\008\000\022\000\
\\011\000\021\000\013\000\020\000\014\000\019\000\015\000\018\000\
\\016\000\017\000\019\000\016\000\025\000\015\000\031\000\014\000\
\\033\000\013\000\034\000\012\000\036\000\011\000\037\000\010\000\
\\046\000\009\000\047\000\008\000\000\000\
\\001\000\004\000\085\000\005\000\038\000\007\000\037\000\017\000\036\000\
\\018\000\035\000\020\000\034\000\021\000\033\000\022\000\032\000\
\\024\000\031\000\025\000\030\000\026\000\029\000\027\000\028\000\
\\029\000\027\000\032\000\084\000\000\000\
\\001\000\004\000\107\000\029\000\083\000\032\000\106\000\039\000\082\000\000\000\
\\001\000\004\000\107\000\032\000\106\000\039\000\082\000\000\000\
\\001\000\005\000\038\000\007\000\037\000\009\000\091\000\017\000\036\000\
\\018\000\035\000\020\000\034\000\021\000\033\000\022\000\032\000\
\\024\000\031\000\025\000\030\000\026\000\029\000\027\000\028\000\
\\029\000\027\000\000\000\
\\001\000\005\000\038\000\007\000\037\000\010\000\124\000\017\000\036\000\
\\018\000\035\000\020\000\034\000\021\000\033\000\022\000\032\000\
\\024\000\031\000\025\000\030\000\026\000\029\000\027\000\028\000\
\\029\000\027\000\000\000\
\\001\000\005\000\038\000\007\000\037\000\012\000\090\000\017\000\036\000\
\\018\000\035\000\020\000\034\000\021\000\033\000\022\000\032\000\
\\024\000\031\000\025\000\030\000\026\000\029\000\027\000\028\000\
\\029\000\027\000\000\000\
\\001\000\005\000\038\000\007\000\037\000\017\000\036\000\018\000\035\000\
\\020\000\034\000\021\000\033\000\022\000\032\000\024\000\031\000\
\\025\000\030\000\026\000\029\000\027\000\028\000\029\000\027\000\
\\030\000\141\000\048\000\141\000\000\000\
\\001\000\005\000\038\000\007\000\037\000\017\000\036\000\018\000\035\000\
\\020\000\034\000\021\000\033\000\022\000\032\000\024\000\031\000\
\\025\000\030\000\026\000\029\000\027\000\028\000\029\000\027\000\
\\035\000\111\000\038\000\110\000\000\000\
\\001\000\005\000\038\000\007\000\037\000\017\000\036\000\018\000\035\000\
\\020\000\034\000\021\000\033\000\022\000\032\000\024\000\031\000\
\\025\000\030\000\026\000\029\000\027\000\028\000\029\000\027\000\
\\035\000\116\000\000\000\
\\001\000\005\000\127\000\007\000\037\000\017\000\036\000\018\000\035\000\
\\020\000\034\000\021\000\033\000\022\000\032\000\024\000\031\000\
\\025\000\030\000\026\000\029\000\027\000\028\000\029\000\027\000\000\000\
\\001\000\005\000\131\000\007\000\037\000\017\000\036\000\018\000\035\000\
\\020\000\034\000\021\000\033\000\022\000\032\000\024\000\031\000\
\\025\000\030\000\026\000\029\000\027\000\028\000\029\000\027\000\000\000\
\\001\000\005\000\138\000\007\000\037\000\017\000\036\000\018\000\035\000\
\\020\000\034\000\021\000\033\000\022\000\032\000\024\000\031\000\
\\025\000\030\000\026\000\029\000\027\000\028\000\029\000\027\000\000\000\
\\001\000\006\000\113\000\000\000\
\\001\000\008\000\022\000\011\000\021\000\013\000\020\000\014\000\019\000\
\\015\000\018\000\016\000\017\000\019\000\016\000\025\000\015\000\
\\029\000\050\000\031\000\014\000\032\000\049\000\033\000\048\000\
\\034\000\012\000\036\000\011\000\037\000\010\000\042\000\047\000\
\\043\000\046\000\044\000\045\000\046\000\009\000\047\000\008\000\000\000\
\\001\000\008\000\022\000\011\000\021\000\013\000\020\000\014\000\019\000\
\\015\000\018\000\016\000\017\000\019\000\016\000\025\000\015\000\
\\031\000\014\000\033\000\013\000\034\000\012\000\036\000\011\000\
\\037\000\010\000\041\000\123\000\046\000\009\000\047\000\008\000\000\000\
\\001\000\008\000\022\000\011\000\021\000\013\000\020\000\014\000\019\000\
\\015\000\018\000\016\000\017\000\019\000\016\000\025\000\015\000\
\\031\000\014\000\033\000\013\000\034\000\012\000\036\000\011\000\
\\037\000\010\000\046\000\009\000\047\000\008\000\000\000\
\\001\000\017\000\094\000\000\000\
\\001\000\017\000\114\000\000\000\
\\001\000\017\000\130\000\039\000\082\000\000\000\
\\001\000\028\000\095\000\000\000\
\\001\000\028\000\102\000\000\000\
\\001\000\028\000\108\000\039\000\082\000\000\000\
\\001\000\029\000\050\000\032\000\080\000\033\000\079\000\042\000\047\000\
\\043\000\046\000\044\000\045\000\000\000\
\\001\000\029\000\050\000\033\000\079\000\042\000\047\000\043\000\046\000\
\\044\000\045\000\000\000\
\\001\000\029\000\083\000\039\000\082\000\000\000\
\\001\000\030\000\089\000\000\000\
\\001\000\032\000\081\000\000\000\
\\001\000\032\000\097\000\000\000\
\\001\000\032\000\105\000\000\000\
\\001\000\032\000\118\000\000\000\
\\001\000\033\000\040\000\000\000\
\\001\000\035\000\111\000\038\000\110\000\000\000\
\\001\000\039\000\082\000\046\000\099\000\000\000\
\\001\000\039\000\128\000\000\000\
\\001\000\040\000\075\000\000\000\
\\001\000\046\000\060\000\000\000\
\\001\000\046\000\061\000\000\000\
\\001\000\046\000\062\000\000\000\
\\001\000\047\000\063\000\000\000\
\\001\000\048\000\000\000\000\000\
\\141\000\005\000\038\000\007\000\037\000\017\000\036\000\018\000\035\000\
\\020\000\034\000\021\000\033\000\022\000\032\000\024\000\031\000\
\\025\000\030\000\026\000\029\000\027\000\028\000\029\000\027\000\000\000\
\\142\000\000\000\
\\143\000\000\000\
\\144\000\000\000\
\\145\000\000\000\
\\146\000\031\000\014\000\033\000\013\000\034\000\012\000\036\000\011\000\
\\037\000\010\000\046\000\009\000\047\000\008\000\000\000\
\\147\000\000\000\
\\148\000\007\000\037\000\017\000\036\000\018\000\035\000\020\000\034\000\
\\021\000\033\000\022\000\032\000\024\000\031\000\025\000\030\000\
\\026\000\029\000\027\000\028\000\029\000\027\000\000\000\
\\149\000\000\000\
\\150\000\029\000\027\000\000\000\
\\151\000\026\000\029\000\027\000\028\000\029\000\027\000\000\000\
\\152\000\029\000\027\000\000\000\
\\153\000\029\000\027\000\000\000\
\\154\000\029\000\027\000\000\000\
\\155\000\029\000\027\000\000\000\
\\156\000\007\000\037\000\017\000\036\000\018\000\035\000\021\000\033\000\
\\022\000\032\000\024\000\031\000\025\000\030\000\026\000\029\000\
\\027\000\028\000\029\000\027\000\000\000\
\\157\000\026\000\029\000\027\000\028\000\029\000\027\000\000\000\
\\158\000\026\000\029\000\027\000\028\000\029\000\027\000\000\000\
\\159\000\029\000\027\000\000\000\
\\160\000\029\000\027\000\000\000\
\\161\000\007\000\037\000\021\000\033\000\022\000\032\000\024\000\031\000\
\\025\000\030\000\026\000\029\000\027\000\028\000\029\000\027\000\000\000\
\\162\000\007\000\037\000\021\000\033\000\022\000\032\000\024\000\031\000\
\\025\000\030\000\026\000\029\000\027\000\028\000\029\000\027\000\000\000\
\\163\000\007\000\037\000\024\000\031\000\025\000\030\000\026\000\029\000\
\\027\000\028\000\029\000\027\000\000\000\
\\164\000\007\000\037\000\024\000\031\000\025\000\030\000\026\000\029\000\
\\027\000\028\000\029\000\027\000\000\000\
\\165\000\007\000\037\000\024\000\031\000\025\000\030\000\026\000\029\000\
\\027\000\028\000\029\000\027\000\000\000\
\\166\000\005\000\038\000\007\000\037\000\017\000\036\000\018\000\035\000\
\\020\000\034\000\021\000\033\000\022\000\032\000\024\000\031\000\
\\025\000\030\000\026\000\029\000\027\000\028\000\029\000\027\000\000\000\
\\167\000\000\000\
\\168\000\000\000\
\\169\000\000\000\
\\170\000\000\000\
\\171\000\000\000\
\\172\000\000\000\
\\173\000\000\000\
\\174\000\000\000\
\\175\000\000\000\
\\176\000\000\000\
\\177\000\000\000\
\\178\000\000\000\
\\179\000\000\000\
\\180\000\004\000\085\000\005\000\038\000\007\000\037\000\017\000\036\000\
\\018\000\035\000\020\000\034\000\021\000\033\000\022\000\032\000\
\\024\000\031\000\025\000\030\000\026\000\029\000\027\000\028\000\
\\029\000\027\000\000\000\
\\181\000\000\000\
\\182\000\000\000\
\\183\000\000\000\
\\184\000\005\000\038\000\007\000\037\000\017\000\036\000\018\000\035\000\
\\020\000\034\000\021\000\033\000\022\000\032\000\024\000\031\000\
\\025\000\030\000\026\000\029\000\027\000\028\000\029\000\027\000\000\000\
\\185\000\000\000\
\\186\000\000\000\
\\187\000\000\000\
\\188\000\004\000\098\000\000\000\
\\189\000\000\000\
\\190\000\000\000\
\\191\000\000\000\
\\192\000\000\000\
\\193\000\000\000\
\\194\000\039\000\082\000\000\000\
\\195\000\000\000\
\\196\000\000\000\
\\197\000\000\000\
\\198\000\000\000\
\\199\000\004\000\107\000\039\000\082\000\000\000\
\\200\000\000\000\
\"
val actionRowNumbers =
"\000\000\069\000\047\000\046\000\
\\042\000\041\000\077\000\070\000\
\\076\000\075\000\031\000\014\000\
\\000\000\016\000\016\000\016\000\
\\016\000\016\000\016\000\016\000\
\\016\000\036\000\037\000\038\000\
\\068\000\039\000\016\000\016\000\
\\016\000\016\000\016\000\016\000\
\\016\000\016\000\016\000\016\000\
\\016\000\035\000\023\000\027\000\
\\091\000\025\000\001\000\097\000\
\\096\000\095\000\014\000\078\000\
\\024\000\026\000\051\000\050\000\
\\055\000\054\000\053\000\052\000\
\\006\000\004\000\031\000\031\000\
\\017\000\020\000\060\000\059\000\
\\058\000\057\000\064\000\063\000\
\\056\000\062\000\061\000\065\000\
\\066\000\016\000\028\000\088\000\
\\033\000\024\000\086\000\073\000\
\\024\000\021\000\072\000\016\000\
\\029\000\002\000\022\000\071\000\
\\032\000\016\000\013\000\018\000\
\\016\000\067\000\009\000\087\000\
\\024\000\090\000\003\000\094\000\
\\030\000\081\000\080\000\092\000\
\\098\000\024\000\093\000\049\000\
\\015\000\083\000\005\000\024\000\
\\016\000\010\000\074\000\089\000\
\\079\000\100\000\099\000\034\000\
\\084\000\085\000\016\000\019\000\
\\011\000\000\000\016\000\048\000\
\\016\000\000\000\007\000\043\000\
\\008\000\012\000\044\000\082\000\
\\000\000\045\000\040\000"
val gotoT =
"\
\\001\000\138\000\002\000\005\000\003\000\004\000\008\000\003\000\
\\009\000\002\000\015\000\001\000\000\000\
\\000\000\
\\000\000\
\\008\000\024\000\015\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\004\000\037\000\000\000\
\\002\000\042\000\005\000\041\000\007\000\040\000\008\000\003\000\
\\009\000\002\000\011\000\039\000\015\000\001\000\000\000\
\\001\000\049\000\002\000\005\000\003\000\004\000\008\000\003\000\
\\009\000\002\000\015\000\001\000\000\000\
\\002\000\050\000\008\000\003\000\009\000\002\000\015\000\001\000\000\000\
\\002\000\051\000\008\000\003\000\009\000\002\000\015\000\001\000\000\000\
\\002\000\052\000\008\000\003\000\009\000\002\000\015\000\001\000\000\000\
\\002\000\053\000\008\000\003\000\009\000\002\000\015\000\001\000\000\000\
\\002\000\054\000\008\000\003\000\009\000\002\000\015\000\001\000\000\000\
\\002\000\055\000\008\000\003\000\009\000\002\000\015\000\001\000\000\000\
\\002\000\056\000\008\000\003\000\009\000\002\000\015\000\001\000\000\000\
\\002\000\057\000\008\000\003\000\009\000\002\000\015\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\062\000\008\000\003\000\009\000\002\000\015\000\001\000\000\000\
\\002\000\063\000\008\000\003\000\009\000\002\000\015\000\001\000\000\000\
\\002\000\064\000\008\000\003\000\009\000\002\000\015\000\001\000\000\000\
\\002\000\065\000\008\000\003\000\009\000\002\000\015\000\001\000\000\000\
\\002\000\066\000\008\000\003\000\009\000\002\000\015\000\001\000\000\000\
\\002\000\067\000\008\000\003\000\009\000\002\000\015\000\001\000\000\000\
\\002\000\068\000\008\000\003\000\009\000\002\000\015\000\001\000\000\000\
\\002\000\069\000\008\000\003\000\009\000\002\000\015\000\001\000\000\000\
\\002\000\070\000\008\000\003\000\009\000\002\000\015\000\001\000\000\000\
\\002\000\071\000\008\000\003\000\009\000\002\000\015\000\001\000\000\000\
\\002\000\072\000\008\000\003\000\009\000\002\000\015\000\001\000\000\000\
\\000\000\
\\005\000\076\000\007\000\040\000\013\000\075\000\014\000\074\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\042\000\005\000\085\000\006\000\084\000\007\000\040\000\
\\008\000\003\000\009\000\002\000\011\000\039\000\015\000\001\000\000\000\
\\000\000\
\\005\000\086\000\007\000\040\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\004\000\090\000\000\000\
\\004\000\091\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\094\000\008\000\003\000\009\000\002\000\015\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\005\000\098\000\006\000\084\000\007\000\040\000\000\000\
\\000\000\
\\000\000\
\\005\000\099\000\007\000\040\000\000\000\
\\000\000\
\\000\000\
\\002\000\102\000\008\000\003\000\009\000\002\000\011\000\101\000\
\\015\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\010\000\107\000\000\000\
\\002\000\110\000\008\000\003\000\009\000\002\000\015\000\001\000\000\000\
\\000\000\
\\000\000\
\\002\000\113\000\008\000\003\000\009\000\002\000\015\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\005\000\076\000\007\000\040\000\013\000\075\000\014\000\115\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\005\000\118\000\006\000\117\000\007\000\040\000\000\000\
\\000\000\
\\000\000\
\\002\000\120\000\008\000\003\000\009\000\002\000\012\000\119\000\
\\015\000\001\000\000\000\
\\000\000\
\\000\000\
\\005\000\123\000\007\000\040\000\000\000\
\\002\000\124\000\008\000\003\000\009\000\002\000\015\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\127\000\008\000\003\000\009\000\002\000\015\000\001\000\000\000\
\\000\000\
\\000\000\
\\001\000\131\000\002\000\130\000\003\000\004\000\008\000\003\000\
\\009\000\002\000\015\000\001\000\000\000\
\\002\000\132\000\008\000\003\000\009\000\002\000\015\000\001\000\000\000\
\\000\000\
\\002\000\133\000\008\000\003\000\009\000\002\000\015\000\001\000\000\000\
\\001\000\134\000\002\000\130\000\003\000\004\000\008\000\003\000\
\\009\000\002\000\015\000\001\000\000\000\
\\000\000\
\\000\000\
\\010\000\135\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\137\000\002\000\130\000\003\000\004\000\008\000\003\000\
\\009\000\002\000\015\000\001\000\000\000\
\\000\000\
\\000\000\
\"
val numstates = 139
val numrules = 60
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle General.Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(List.map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = int
type arg = unit
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit ->  unit
 | Nat of unit ->  (int) | Name of unit ->  (string)
 | Const of unit ->  (expr)
 | Params of unit ->  ( (  plcType * string  )  list)
 | TypedVar of unit ->  (plcType*string) | CondExpr of unit ->  (expr)
 | Comps of unit ->  (expr list)
 | MatchExpr of unit ->  ( ( expr option * expr )  list)
 | AppExpr of unit ->  (expr) | AtomicExpr of unit ->  (expr)
 | AtomicType of unit ->  (plcType) | Types of unit ->  (plcType list)
 | Type of unit ->  (plcType)
 | Args of unit ->  ( (  plcType * string  )  list)
 | Decl of unit ->  (expr) | Expr of unit ->  (expr)
 | Prog of unit ->  (expr)
end
type svalue = MlyValue.svalue
type result = expr
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn _ => false
val preferred_change : (term list * term list) list = 
nil
val noShift = 
fn (T 47) => true | _ => false
val showTerminal =
fn (T 0) => "VAR"
  | (T 1) => "FUN"
  | (T 2) => "FUNREC"
  | (T 3) => "COMMA"
  | (T 4) => "SEMIC"
  | (T 5) => "COLON"
  | (T 6) => "DCOLON"
  | (T 7) => "IF"
  | (T 8) => "THEN"
  | (T 9) => "ELSE"
  | (T 10) => "MATCH"
  | (T 11) => "WITH"
  | (T 12) => "HEAD"
  | (T 13) => "TAIL"
  | (T 14) => "ISE"
  | (T 15) => "PRINT"
  | (T 16) => "EQ"
  | (T 17) => "NEQ"
  | (T 18) => "NOT"
  | (T 19) => "AND"
  | (T 20) => "LT"
  | (T 21) => "LTE"
  | (T 22) => "GTE"
  | (T 23) => "PLUS"
  | (T 24) => "MINUS"
  | (T 25) => "MULTI"
  | (T 26) => "DIV"
  | (T 27) => "RBRA"
  | (T 28) => "LBRA"
  | (T 29) => "RKEY"
  | (T 30) => "LKEY"
  | (T 31) => "RPAR"
  | (T 32) => "LPAR"
  | (T 33) => "FN"
  | (T 34) => "END"
  | (T 35) => "TRUE"
  | (T 36) => "FALSE"
  | (T 37) => "PIPE"
  | (T 38) => "ARROW"
  | (T 39) => "DARROW"
  | (T 40) => "USCORE"
  | (T 41) => "TNIL"
  | (T 42) => "TBOOL"
  | (T 43) => "TINT"
  | (T 44) => "NIL"
  | (T 45) => "Name"
  | (T 46) => "Nat"
  | (T 47) => "EOF"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 47) $$ (T 44) $$ (T 43) $$ (T 42) $$ (T 41) $$ (T 40) $$ (T 39)
 $$ (T 38) $$ (T 37) $$ (T 36) $$ (T 35) $$ (T 34) $$ (T 33) $$ (T 32)
 $$ (T 31) $$ (T 30) $$ (T 29) $$ (T 28) $$ (T 27) $$ (T 26) $$ (T 25)
 $$ (T 24) $$ (T 23) $$ (T 22) $$ (T 21) $$ (T 20) $$ (T 19) $$ (T 18)
 $$ (T 17) $$ (T 16) $$ (T 15) $$ (T 14) $$ (T 13) $$ (T 12) $$ (T 11)
 $$ (T 10) $$ (T 9) $$ (T 8) $$ (T 7) $$ (T 6) $$ (T 5) $$ (T 4) $$ 
(T 3) $$ (T 2) $$ (T 1) $$ (T 0)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.Expr Expr1, Expr1left, Expr1right)) :: 
rest671)) => let val  result = MlyValue.Prog (fn _ => let val  (Expr
 as Expr1) = Expr1 ()
 in (Expr)
end)
 in ( LrTable.NT 0, ( result, Expr1left, Expr1right), rest671)
end
|  ( 1, ( ( _, ( MlyValue.Decl Decl1, Decl1left, Decl1right)) :: 
rest671)) => let val  result = MlyValue.Prog (fn _ => let val  (Decl
 as Decl1) = Decl1 ()
 in (Decl)
end)
 in ( LrTable.NT 0, ( result, Decl1left, Decl1right), rest671)
end
|  ( 2, ( ( _, ( MlyValue.Prog Prog1, _, Prog1right)) :: _ :: ( _, ( 
MlyValue.Expr Expr1, _, _)) :: _ :: ( _, ( MlyValue.Name Name1, _, _))
 :: ( _, ( _, VAR1left, _)) :: rest671)) => let val  result = 
MlyValue.Decl (fn _ => let val  (Name as Name1) = Name1 ()
 val  (Expr as Expr1) = Expr1 ()
 val  (Prog as Prog1) = Prog1 ()
 in (Let(Name, Expr, Prog))
end)
 in ( LrTable.NT 2, ( result, VAR1left, Prog1right), rest671)
end
|  ( 3, ( ( _, ( MlyValue.Prog Prog1, _, Prog1right)) :: _ :: ( _, ( 
MlyValue.Expr Expr1, _, _)) :: _ :: ( _, ( MlyValue.Args Args1, _, _))
 :: ( _, ( MlyValue.Name Name1, _, _)) :: ( _, ( _, FUN1left, _)) :: 
rest671)) => let val  result = MlyValue.Decl (fn _ => let val  (Name
 as Name1) = Name1 ()
 val  (Args as Args1) = Args1 ()
 val  (Expr as Expr1) = Expr1 ()
 val  (Prog as Prog1) = Prog1 ()
 in (Let(Name, makeAnon(Args, Expr), Prog))
end)
 in ( LrTable.NT 2, ( result, FUN1left, Prog1right), rest671)
end
|  ( 4, ( ( _, ( MlyValue.Prog Prog1, _, Prog1right)) :: _ :: ( _, ( 
MlyValue.Expr Expr1, _, _)) :: _ :: ( _, ( MlyValue.Type Type1, _, _))
 :: _ :: ( _, ( MlyValue.Args Args1, _, _)) :: ( _, ( MlyValue.Name 
Name1, _, _)) :: ( _, ( _, FUNREC1left, _)) :: rest671)) => let val  
result = MlyValue.Decl (fn _ => let val  (Name as Name1) = Name1 ()
 val  (Args as Args1) = Args1 ()
 val  (Type as Type1) = Type1 ()
 val  (Expr as Expr1) = Expr1 ()
 val  (Prog as Prog1) = Prog1 ()
 in (makeFun(Name, Args, Type, Expr, Prog))
end)
 in ( LrTable.NT 2, ( result, FUNREC1left, Prog1right), rest671)
end
|  ( 5, ( ( _, ( MlyValue.AtomicExpr AtomicExpr1, AtomicExpr1left, 
AtomicExpr1right)) :: rest671)) => let val  result = MlyValue.Expr (fn
 _ => let val  (AtomicExpr as AtomicExpr1) = AtomicExpr1 ()
 in (AtomicExpr)
end)
 in ( LrTable.NT 1, ( result, AtomicExpr1left, AtomicExpr1right), 
rest671)
end
|  ( 6, ( ( _, ( MlyValue.AppExpr AppExpr1, AppExpr1left, 
AppExpr1right)) :: rest671)) => let val  result = MlyValue.Expr (fn _
 => let val  (AppExpr as AppExpr1) = AppExpr1 ()
 in (AppExpr)
end)
 in ( LrTable.NT 1, ( result, AppExpr1left, AppExpr1right), rest671)

end
|  ( 7, ( ( _, ( MlyValue.Expr Expr3, _, Expr3right)) :: _ :: ( _, ( 
MlyValue.Expr Expr2, _, _)) :: _ :: ( _, ( MlyValue.Expr Expr1, _, _))
 :: ( _, ( _, IF1left, _)) :: rest671)) => let val  result = 
MlyValue.Expr (fn _ => let val  Expr1 = Expr1 ()
 val  Expr2 = Expr2 ()
 val  Expr3 = Expr3 ()
 in (If(Expr1, Expr2, Expr3))
end)
 in ( LrTable.NT 1, ( result, IF1left, Expr3right), rest671)
end
|  ( 8, ( ( _, ( MlyValue.MatchExpr MatchExpr1, _, MatchExpr1right))
 :: _ :: ( _, ( MlyValue.Expr Expr1, _, _)) :: ( _, ( _, MATCH1left, _
)) :: rest671)) => let val  result = MlyValue.Expr (fn _ => let val  (
Expr as Expr1) = Expr1 ()
 val  (MatchExpr as MatchExpr1) = MatchExpr1 ()
 in (Match(Expr, MatchExpr))
end)
 in ( LrTable.NT 1, ( result, MATCH1left, MatchExpr1right), rest671)

end
|  ( 9, ( ( _, ( MlyValue.Expr Expr1, _, Expr1right)) :: ( _, ( _, 
NOT1left, _)) :: rest671)) => let val  result = MlyValue.Expr (fn _ =>
 let val  (Expr as Expr1) = Expr1 ()
 in (Prim1("!", Expr))
end)
 in ( LrTable.NT 1, ( result, NOT1left, Expr1right), rest671)
end
|  ( 10, ( ( _, ( MlyValue.Expr Expr1, _, Expr1right)) :: ( _, ( _, 
MINUS1left, _)) :: rest671)) => let val  result = MlyValue.Expr (fn _
 => let val  (Expr as Expr1) = Expr1 ()
 in (Prim1("-", Expr))
end)
 in ( LrTable.NT 1, ( result, MINUS1left, Expr1right), rest671)
end
|  ( 11, ( ( _, ( MlyValue.Expr Expr1, _, Expr1right)) :: ( _, ( _, 
HEAD1left, _)) :: rest671)) => let val  result = MlyValue.Expr (fn _
 => let val  (Expr as Expr1) = Expr1 ()
 in (Prim1("hd", Expr))
end)
 in ( LrTable.NT 1, ( result, HEAD1left, Expr1right), rest671)
end
|  ( 12, ( ( _, ( MlyValue.Expr Expr1, _, Expr1right)) :: ( _, ( _, 
TAIL1left, _)) :: rest671)) => let val  result = MlyValue.Expr (fn _
 => let val  (Expr as Expr1) = Expr1 ()
 in (Prim1("tl", Expr))
end)
 in ( LrTable.NT 1, ( result, TAIL1left, Expr1right), rest671)
end
|  ( 13, ( ( _, ( MlyValue.Expr Expr1, _, Expr1right)) :: ( _, ( _, 
ISE1left, _)) :: rest671)) => let val  result = MlyValue.Expr (fn _ =>
 let val  (Expr as Expr1) = Expr1 ()
 in (Prim1("null", Expr))
end)
 in ( LrTable.NT 1, ( result, ISE1left, Expr1right), rest671)
end
|  ( 14, ( ( _, ( MlyValue.Expr Expr1, _, Expr1right)) :: ( _, ( _, 
PRINT1left, _)) :: rest671)) => let val  result = MlyValue.Expr (fn _
 => let val  (Expr as Expr1) = Expr1 ()
 in (Prim1("print", Expr))
end)
 in ( LrTable.NT 1, ( result, PRINT1left, Expr1right), rest671)
end
|  ( 15, ( ( _, ( MlyValue.Expr Expr2, _, Expr2right)) :: _ :: ( _, ( 
MlyValue.Expr Expr1, Expr1left, _)) :: rest671)) => let val  result = 
MlyValue.Expr (fn _ => let val  Expr1 = Expr1 ()
 val  Expr2 = Expr2 ()
 in (Prim2("&&", Expr1, Expr2))
end)
 in ( LrTable.NT 1, ( result, Expr1left, Expr2right), rest671)
end
|  ( 16, ( ( _, ( MlyValue.Expr Expr2, _, Expr2right)) :: _ :: ( _, ( 
MlyValue.Expr Expr1, Expr1left, _)) :: rest671)) => let val  result = 
MlyValue.Expr (fn _ => let val  Expr1 = Expr1 ()
 val  Expr2 = Expr2 ()
 in (Prim2("+", Expr1, Expr2))
end)
 in ( LrTable.NT 1, ( result, Expr1left, Expr2right), rest671)
end
|  ( 17, ( ( _, ( MlyValue.Expr Expr2, _, Expr2right)) :: _ :: ( _, ( 
MlyValue.Expr Expr1, Expr1left, _)) :: rest671)) => let val  result = 
MlyValue.Expr (fn _ => let val  Expr1 = Expr1 ()
 val  Expr2 = Expr2 ()
 in (Prim2("-", Expr1, Expr2))
end)
 in ( LrTable.NT 1, ( result, Expr1left, Expr2right), rest671)
end
|  ( 18, ( ( _, ( MlyValue.Expr Expr2, _, Expr2right)) :: _ :: ( _, ( 
MlyValue.Expr Expr1, Expr1left, _)) :: rest671)) => let val  result = 
MlyValue.Expr (fn _ => let val  Expr1 = Expr1 ()
 val  Expr2 = Expr2 ()
 in (Prim2("*", Expr1, Expr2))
end)
 in ( LrTable.NT 1, ( result, Expr1left, Expr2right), rest671)
end
|  ( 19, ( ( _, ( MlyValue.Expr Expr2, _, Expr2right)) :: _ :: ( _, ( 
MlyValue.Expr Expr1, Expr1left, _)) :: rest671)) => let val  result = 
MlyValue.Expr (fn _ => let val  Expr1 = Expr1 ()
 val  Expr2 = Expr2 ()
 in (Prim2("/", Expr1, Expr2))
end)
 in ( LrTable.NT 1, ( result, Expr1left, Expr2right), rest671)
end
|  ( 20, ( ( _, ( MlyValue.Expr Expr2, _, Expr2right)) :: _ :: ( _, ( 
MlyValue.Expr Expr1, Expr1left, _)) :: rest671)) => let val  result = 
MlyValue.Expr (fn _ => let val  Expr1 = Expr1 ()
 val  Expr2 = Expr2 ()
 in (Prim2("=", Expr1, Expr2))
end)
 in ( LrTable.NT 1, ( result, Expr1left, Expr2right), rest671)
end
|  ( 21, ( ( _, ( MlyValue.Expr Expr2, _, Expr2right)) :: _ :: ( _, ( 
MlyValue.Expr Expr1, Expr1left, _)) :: rest671)) => let val  result = 
MlyValue.Expr (fn _ => let val  Expr1 = Expr1 ()
 val  Expr2 = Expr2 ()
 in (Prim2("!=", Expr1, Expr2))
end)
 in ( LrTable.NT 1, ( result, Expr1left, Expr2right), rest671)
end
|  ( 22, ( ( _, ( MlyValue.Expr Expr2, _, Expr2right)) :: _ :: ( _, ( 
MlyValue.Expr Expr1, Expr1left, _)) :: rest671)) => let val  result = 
MlyValue.Expr (fn _ => let val  Expr1 = Expr1 ()
 val  Expr2 = Expr2 ()
 in (Prim2("<", Expr1, Expr2))
end)
 in ( LrTable.NT 1, ( result, Expr1left, Expr2right), rest671)
end
|  ( 23, ( ( _, ( MlyValue.Expr Expr2, _, Expr2right)) :: _ :: ( _, ( 
MlyValue.Expr Expr1, Expr1left, _)) :: rest671)) => let val  result = 
MlyValue.Expr (fn _ => let val  Expr1 = Expr1 ()
 val  Expr2 = Expr2 ()
 in (Prim2("<=", Expr1, Expr2))
end)
 in ( LrTable.NT 1, ( result, Expr1left, Expr2right), rest671)
end
|  ( 24, ( ( _, ( MlyValue.Expr Expr2, _, Expr2right)) :: _ :: ( _, ( 
MlyValue.Expr Expr1, Expr1left, _)) :: rest671)) => let val  result = 
MlyValue.Expr (fn _ => let val  Expr1 = Expr1 ()
 val  Expr2 = Expr2 ()
 in (Prim2("::", Expr1, Expr2))
end)
 in ( LrTable.NT 1, ( result, Expr1left, Expr2right), rest671)
end
|  ( 25, ( ( _, ( MlyValue.Expr Expr2, _, Expr2right)) :: _ :: ( _, ( 
MlyValue.Expr Expr1, Expr1left, _)) :: rest671)) => let val  result = 
MlyValue.Expr (fn _ => let val  Expr1 = Expr1 ()
 val  Expr2 = Expr2 ()
 in (Prim2(";", Expr1, Expr2))
end)
 in ( LrTable.NT 1, ( result, Expr1left, Expr2right), rest671)
end
|  ( 26, ( ( _, ( _, _, RBRA1right)) :: ( _, ( MlyValue.Nat Nat1, _, _
)) :: _ :: ( _, ( MlyValue.Expr Expr1, Expr1left, _)) :: rest671)) =>
 let val  result = MlyValue.Expr (fn _ => let val  (Expr as Expr1) = 
Expr1 ()
 val  (Nat as Nat1) = Nat1 ()
 in (Item(Nat, Expr))
end)
 in ( LrTable.NT 1, ( result, Expr1left, RBRA1right), rest671)
end
|  ( 27, ( ( _, ( MlyValue.AtomicExpr AtomicExpr2, _, AtomicExpr2right
)) :: ( _, ( MlyValue.AtomicExpr AtomicExpr1, AtomicExpr1left, _)) :: 
rest671)) => let val  result = MlyValue.AppExpr (fn _ => let val  
AtomicExpr1 = AtomicExpr1 ()
 val  AtomicExpr2 = AtomicExpr2 ()
 in (Call(AtomicExpr1, AtomicExpr2))
end)
 in ( LrTable.NT 8, ( result, AtomicExpr1left, AtomicExpr2right), 
rest671)
end
|  ( 28, ( ( _, ( MlyValue.Const Const1, Const1left, Const1right)) :: 
rest671)) => let val  result = MlyValue.AtomicExpr (fn _ => let val  (
Const as Const1) = Const1 ()
 in (Const)
end)
 in ( LrTable.NT 7, ( result, Const1left, Const1right), rest671)
end
|  ( 29, ( ( _, ( MlyValue.Name Name1, Name1left, Name1right)) :: 
rest671)) => let val  result = MlyValue.AtomicExpr (fn _ => let val  (
Name as Name1) = Name1 ()
 in (Var(Name))
end)
 in ( LrTable.NT 7, ( result, Name1left, Name1right), rest671)
end
|  ( 30, ( ( _, ( _, _, RKEY1right)) :: ( _, ( MlyValue.Prog Prog1, _,
 _)) :: ( _, ( _, LKEY1left, _)) :: rest671)) => let val  result = 
MlyValue.AtomicExpr (fn _ => let val  (Prog as Prog1) = Prog1 ()
 in (Prog)
end)
 in ( LrTable.NT 7, ( result, LKEY1left, RKEY1right), rest671)
end
|  ( 31, ( ( _, ( _, _, RPAR1right)) :: ( _, ( MlyValue.Expr Expr1, _,
 _)) :: ( _, ( _, LPAR1left, _)) :: rest671)) => let val  result = 
MlyValue.AtomicExpr (fn _ => let val  (Expr as Expr1) = Expr1 ()
 in (Expr)
end)
 in ( LrTable.NT 7, ( result, LPAR1left, RPAR1right), rest671)
end
|  ( 32, ( ( _, ( _, _, RPAR1right)) :: ( _, ( MlyValue.Comps Comps1,
 _, _)) :: ( _, ( _, LPAR1left, _)) :: rest671)) => let val  result = 
MlyValue.AtomicExpr (fn _ => let val  (Comps as Comps1) = Comps1 ()
 in (List Comps)
end)
 in ( LrTable.NT 7, ( result, LPAR1left, RPAR1right), rest671)
end
|  ( 33, ( ( _, ( _, _, END1right)) :: ( _, ( MlyValue.Expr Expr1, _,
 _)) :: _ :: ( _, ( MlyValue.Args Args1, _, _)) :: ( _, ( _, FN1left,
 _)) :: rest671)) => let val  result = MlyValue.AtomicExpr (fn _ =>
 let val  (Args as Args1) = Args1 ()
 val  (Expr as Expr1) = Expr1 ()
 in (makeAnon(Args, Expr))
end)
 in ( LrTable.NT 7, ( result, FN1left, END1right), rest671)
end
|  ( 34, ( ( _, ( _, TRUE1left, TRUE1right)) :: rest671)) => let val  
result = MlyValue.Const (fn _ => (ConB(true)))
 in ( LrTable.NT 14, ( result, TRUE1left, TRUE1right), rest671)
end
|  ( 35, ( ( _, ( _, FALSE1left, FALSE1right)) :: rest671)) => let
 val  result = MlyValue.Const (fn _ => (ConB(false)))
 in ( LrTable.NT 14, ( result, FALSE1left, FALSE1right), rest671)
end
|  ( 36, ( ( _, ( MlyValue.Nat Nat1, Nat1left, Nat1right)) :: rest671)
) => let val  result = MlyValue.Const (fn _ => let val  (Nat as Nat1)
 = Nat1 ()
 in (ConI(Nat))
end)
 in ( LrTable.NT 14, ( result, Nat1left, Nat1right), rest671)
end
|  ( 37, ( ( _, ( _, _, RPAR1right)) :: ( _, ( _, LPAR1left, _)) :: 
rest671)) => let val  result = MlyValue.Const (fn _ => (List []))
 in ( LrTable.NT 14, ( result, LPAR1left, RPAR1right), rest671)
end
|  ( 38, ( ( _, ( _, _, RPAR1right)) :: _ :: _ :: ( _, ( MlyValue.Type
 Type1, _, _)) :: ( _, ( _, LPAR1left, _)) :: rest671)) => let val  
result = MlyValue.Const (fn _ => let val  (Type as Type1) = Type1 ()
 in (ESeq(Type))
end)
 in ( LrTable.NT 14, ( result, LPAR1left, RPAR1right), rest671)
end
|  ( 39, ( ( _, ( MlyValue.Expr Expr2, _, Expr2right)) :: _ :: ( _, ( 
MlyValue.Expr Expr1, Expr1left, _)) :: rest671)) => let val  result = 
MlyValue.Comps (fn _ => let val  Expr1 = Expr1 ()
 val  Expr2 = Expr2 ()
 in ([Expr1, Expr2])
end)
 in ( LrTable.NT 10, ( result, Expr1left, Expr2right), rest671)
end
|  ( 40, ( ( _, ( MlyValue.Comps Comps1, _, Comps1right)) :: _ :: ( _,
 ( MlyValue.Expr Expr1, Expr1left, _)) :: rest671)) => let val  result
 = MlyValue.Comps (fn _ => let val  (Expr as Expr1) = Expr1 ()
 val  (Comps as Comps1) = Comps1 ()
 in ([Expr] @ Comps)
end)
 in ( LrTable.NT 10, ( result, Expr1left, Comps1right), rest671)
end
|  ( 41, ( ( _, ( MlyValue.MatchExpr MatchExpr1, _, MatchExpr1right))
 :: ( _, ( MlyValue.Expr Expr1, _, _)) :: _ :: ( _, ( 
MlyValue.CondExpr CondExpr1, _, _)) :: ( _, ( _, PIPE1left, _)) :: 
rest671)) => let val  result = MlyValue.MatchExpr (fn _ => let val  (
CondExpr as CondExpr1) = CondExpr1 ()
 val  (Expr as Expr1) = Expr1 ()
 val  (MatchExpr as MatchExpr1) = MatchExpr1 ()
 in ([(CondExpr, Expr)] @ MatchExpr)
end)
 in ( LrTable.NT 9, ( result, PIPE1left, MatchExpr1right), rest671)

end
|  ( 42, ( ( _, ( _, END1left, END1right)) :: rest671)) => let val  
result = MlyValue.MatchExpr (fn _ => ([]))
 in ( LrTable.NT 9, ( result, END1left, END1right), rest671)
end
|  ( 43, ( ( _, ( MlyValue.Expr Expr1, Expr1left, Expr1right)) :: 
rest671)) => let val  result = MlyValue.CondExpr (fn _ => let val  (
Expr as Expr1) = Expr1 ()
 in (SOME(Expr))
end)
 in ( LrTable.NT 11, ( result, Expr1left, Expr1right), rest671)
end
|  ( 44, ( ( _, ( _, USCORE1left, USCORE1right)) :: rest671)) => let
 val  result = MlyValue.CondExpr (fn _ => (NONE))
 in ( LrTable.NT 11, ( result, USCORE1left, USCORE1right), rest671)

end
|  ( 45, ( ( _, ( _, _, RPAR1right)) :: ( _, ( _, LPAR1left, _)) :: 
rest671)) => let val  result = MlyValue.Args (fn _ => ([]))
 in ( LrTable.NT 3, ( result, LPAR1left, RPAR1right), rest671)
end
|  ( 46, ( ( _, ( _, _, RPAR1right)) :: ( _, ( MlyValue.Params Params1
, _, _)) :: ( _, ( _, LPAR1left, _)) :: rest671)) => let val  result =
 MlyValue.Args (fn _ => let val  (Params as Params1) = Params1 ()
 in (Params)
end)
 in ( LrTable.NT 3, ( result, LPAR1left, RPAR1right), rest671)
end
|  ( 47, ( ( _, ( MlyValue.TypedVar TypedVar1, TypedVar1left, 
TypedVar1right)) :: rest671)) => let val  result = MlyValue.Params (fn
 _ => let val  (TypedVar as TypedVar1) = TypedVar1 ()
 in ([TypedVar])
end)
 in ( LrTable.NT 13, ( result, TypedVar1left, TypedVar1right), rest671
)
end
|  ( 48, ( ( _, ( MlyValue.Params Params1, _, Params1right)) :: _ :: (
 _, ( MlyValue.TypedVar TypedVar1, TypedVar1left, _)) :: rest671)) =>
 let val  result = MlyValue.Params (fn _ => let val  (TypedVar as 
TypedVar1) = TypedVar1 ()
 val  (Params as Params1) = Params1 ()
 in ([TypedVar] @ Params)
end)
 in ( LrTable.NT 13, ( result, TypedVar1left, Params1right), rest671)

end
|  ( 49, ( ( _, ( MlyValue.Name Name1, _, Name1right)) :: ( _, ( 
MlyValue.Type Type1, Type1left, _)) :: rest671)) => let val  result = 
MlyValue.TypedVar (fn _ => let val  (Type as Type1) = Type1 ()
 val  (Name as Name1) = Name1 ()
 in (Type, Name)
end)
 in ( LrTable.NT 12, ( result, Type1left, Name1right), rest671)
end
|  ( 50, ( ( _, ( MlyValue.AtomicType AtomicType1, AtomicType1left, 
AtomicType1right)) :: rest671)) => let val  result = MlyValue.Type (fn
 _ => let val  (AtomicType as AtomicType1) = AtomicType1 ()
 in (AtomicType)
end)
 in ( LrTable.NT 4, ( result, AtomicType1left, AtomicType1right), 
rest671)
end
|  ( 51, ( ( _, ( _, _, RPAR1right)) :: ( _, ( MlyValue.Types Types1,
 _, _)) :: ( _, ( _, LPAR1left, _)) :: rest671)) => let val  result = 
MlyValue.Type (fn _ => let val  (Types as Types1) = Types1 ()
 in (ListT (Types))
end)
 in ( LrTable.NT 4, ( result, LPAR1left, RPAR1right), rest671)
end
|  ( 52, ( ( _, ( _, _, RBRA1right)) :: ( _, ( MlyValue.Type Type1, _,
 _)) :: ( _, ( _, LBRA1left, _)) :: rest671)) => let val  result = 
MlyValue.Type (fn _ => let val  (Type as Type1) = Type1 ()
 in (SeqT(Type))
end)
 in ( LrTable.NT 4, ( result, LBRA1left, RBRA1right), rest671)
end
|  ( 53, ( ( _, ( MlyValue.Type Type2, _, Type2right)) :: _ :: ( _, ( 
MlyValue.Type Type1, Type1left, _)) :: rest671)) => let val  result = 
MlyValue.Type (fn _ => let val  Type1 = Type1 ()
 val  Type2 = Type2 ()
 in (FunT(Type1, Type2))
end)
 in ( LrTable.NT 4, ( result, Type1left, Type2right), rest671)
end
|  ( 54, ( ( _, ( _, TNIL1left, TNIL1right)) :: rest671)) => let val  
result = MlyValue.AtomicType (fn _ => (ListT []))
 in ( LrTable.NT 6, ( result, TNIL1left, TNIL1right), rest671)
end
|  ( 55, ( ( _, ( _, TBOOL1left, TBOOL1right)) :: rest671)) => let
 val  result = MlyValue.AtomicType (fn _ => (BoolT))
 in ( LrTable.NT 6, ( result, TBOOL1left, TBOOL1right), rest671)
end
|  ( 56, ( ( _, ( _, TINT1left, TINT1right)) :: rest671)) => let val  
result = MlyValue.AtomicType (fn _ => (IntT))
 in ( LrTable.NT 6, ( result, TINT1left, TINT1right), rest671)
end
|  ( 57, ( ( _, ( _, _, RPAR1right)) :: ( _, ( MlyValue.Type Type1, _,
 _)) :: ( _, ( _, LPAR1left, _)) :: rest671)) => let val  result = 
MlyValue.AtomicType (fn _ => let val  (Type as Type1) = Type1 ()
 in (Type)
end)
 in ( LrTable.NT 6, ( result, LPAR1left, RPAR1right), rest671)
end
|  ( 58, ( ( _, ( MlyValue.Type Type2, _, Type2right)) :: _ :: ( _, ( 
MlyValue.Type Type1, Type1left, _)) :: rest671)) => let val  result = 
MlyValue.Types (fn _ => let val  Type1 = Type1 ()
 val  Type2 = Type2 ()
 in ([Type1, Type2])
end)
 in ( LrTable.NT 5, ( result, Type1left, Type2right), rest671)
end
|  ( 59, ( ( _, ( MlyValue.Types Types1, _, Types1right)) :: _ :: ( _,
 ( MlyValue.Type Type1, Type1left, _)) :: rest671)) => let val  result
 = MlyValue.Types (fn _ => let val  (Type as Type1) = Type1 ()
 val  (Types as Types1) = Types1 ()
 in ([Type] @ Types)
end)
 in ( LrTable.NT 5, ( result, Type1left, Types1right), rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.Prog x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : PlcParser_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun VAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.VOID,p1,p2))
fun FUN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.VOID,p1,p2))
fun FUNREC (p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.VOID,p1,p2))
fun COMMA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.VOID,p1,p2))
fun SEMIC (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun COLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun DCOLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun IF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun THEN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun ELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun MATCH (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun WITH (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun HEAD (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun TAIL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
fun ISE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.VOID,p1,p2))
fun PRINT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(
ParserData.MlyValue.VOID,p1,p2))
fun EQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(
ParserData.MlyValue.VOID,p1,p2))
fun NEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(
ParserData.MlyValue.VOID,p1,p2))
fun NOT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(
ParserData.MlyValue.VOID,p1,p2))
fun AND (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(
ParserData.MlyValue.VOID,p1,p2))
fun LT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(
ParserData.MlyValue.VOID,p1,p2))
fun LTE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(
ParserData.MlyValue.VOID,p1,p2))
fun GTE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(
ParserData.MlyValue.VOID,p1,p2))
fun PLUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(
ParserData.MlyValue.VOID,p1,p2))
fun MINUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(
ParserData.MlyValue.VOID,p1,p2))
fun MULTI (p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(
ParserData.MlyValue.VOID,p1,p2))
fun DIV (p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(
ParserData.MlyValue.VOID,p1,p2))
fun RBRA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 27,(
ParserData.MlyValue.VOID,p1,p2))
fun LBRA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 28,(
ParserData.MlyValue.VOID,p1,p2))
fun RKEY (p1,p2) = Token.TOKEN (ParserData.LrTable.T 29,(
ParserData.MlyValue.VOID,p1,p2))
fun LKEY (p1,p2) = Token.TOKEN (ParserData.LrTable.T 30,(
ParserData.MlyValue.VOID,p1,p2))
fun RPAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 31,(
ParserData.MlyValue.VOID,p1,p2))
fun LPAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 32,(
ParserData.MlyValue.VOID,p1,p2))
fun FN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 33,(
ParserData.MlyValue.VOID,p1,p2))
fun END (p1,p2) = Token.TOKEN (ParserData.LrTable.T 34,(
ParserData.MlyValue.VOID,p1,p2))
fun TRUE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 35,(
ParserData.MlyValue.VOID,p1,p2))
fun FALSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 36,(
ParserData.MlyValue.VOID,p1,p2))
fun PIPE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 37,(
ParserData.MlyValue.VOID,p1,p2))
fun ARROW (p1,p2) = Token.TOKEN (ParserData.LrTable.T 38,(
ParserData.MlyValue.VOID,p1,p2))
fun DARROW (p1,p2) = Token.TOKEN (ParserData.LrTable.T 39,(
ParserData.MlyValue.VOID,p1,p2))
fun USCORE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 40,(
ParserData.MlyValue.VOID,p1,p2))
fun TNIL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 41,(
ParserData.MlyValue.VOID,p1,p2))
fun TBOOL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 42,(
ParserData.MlyValue.VOID,p1,p2))
fun TINT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 43,(
ParserData.MlyValue.VOID,p1,p2))
fun NIL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 44,(
ParserData.MlyValue.VOID,p1,p2))
fun Name (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 45,(
ParserData.MlyValue.Name (fn () => i),p1,p2))
fun Nat (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 46,(
ParserData.MlyValue.Nat (fn () => i),p1,p2))
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 47,(
ParserData.MlyValue.VOID,p1,p2))
end
end
