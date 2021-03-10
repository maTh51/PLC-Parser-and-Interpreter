(* Plc Lexer *)

(* User declarations *)

open Tokens
type pos = int
type slvalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult = (slvalue, pos)token

(* A function to print a message error on the screen. *)
val error = fn x => TextIO.output(TextIO.stdOut, x ^ "\n")
val lineNumber = ref 0

(* Get the current line being read. *)
fun getLineAsString() =
    let
        val lineNum = !lineNumber
    in
        Int.toString lineNum
    end

(* Define what to do when the end of the file is reached. *)
fun eof () = Tokens.EOF(0,0)

fun strToInt s =
	case Int.fromString s of
        SOME i => i
	  | NONE => raise Fail ("Could not convert string '" ^ s ^ "' to integer")

fun keyword (s, lpos, rpos) =
    case s of
        "var" => VAR(lpos, rpos)
        "fun" => FUN(lpos, rpos)
        "fun rec" => FUNREC(lpos, rpos)
        "if" => IF(lpos, rpos)
        "then" => THEN(lpos, rpos)
        "else" => ELSE(lpos, rpos)
        "match" => MATCH(lpos, rpos)
        "with" => WITH(lpos, rpos)
        "hd" => HEAD(lpos, rpos)
        "tl" => TAIL(lpos, rpos)
        "ise" => ISE(lpos, rpos)
        "print" => PRINT(lpos, rpos)
        "fn" => FN(lpos, rpos)
        "end" => END(lpos, rpos)
        "true" => TRUE(lpos, rpos)
        "false" => FALSE(lpos, rpos)
        | _ => Name(s, lpos, rpos)

(* Initialize the lexer. *)
fun init() = ()
%%
%header (functor PlcLexerFun(structure Tokens: PlcParser_TOKENS));
alpha = [A-Za-z];
digit=[0-9];
whitespace=[\ \t];
identifier=[a-zA-Z_][a-zA-Z_0-9]*;
%%

\n => (lineNumber := !lineNumber + 1; lex());
{whitespace}+ => (lex());
{digit}+ => (Const(yytext, yypos, yypos));
{identifier} => (keyword(yytext, yypos, yypos));
"," => (COMMA(yypos, yypos));
";" => (SEMIC(yypos, yypos));
":" => (COLON(yypos, yypos));
"::" => (DCOLON(yypos, yypos));
"=" => (EQ(yypos, yypos));
"!=" => (NEQ(yypos, yypos));
"!" => (NOT(yypos, yypos));
"&&" => (AND(yypos, yypos));
"<" => (LT(yypos, yypos));
"<=" => (LTE(yypos, yypos));
"+" => (PLUS(yypos, yypos));
"-" => (MINUS(yypos, yypos));
"*" => (MULTI(yypos, yypos));
"/" => (DIV(yypos, yypos));
"[" => (LBRA(yypos, yypos));
"]" => (RBRA(yypos, yypos));
"{" => (LKEY(yypos, yypos));
"}" => (RKEY(yypos, yypos));
"(" => (LPAR(yypos, yypos));
")" => (RPAR(yypos, yypos));
"|" => (PIPE(yypos, yypos));
"->" => (ARROW(yypos, yypos));
"=>" => (DARROW(yypos, yypos));
"_" => (USCORE(yypos, yypos));
. => (error("\n*** Lexer error: bad character ***\n");
      raise Fail("Lexer error: bad character " ^ yytext));