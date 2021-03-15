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
          "Bool" => TBOOL(lpos, rpos)
        | "else" => ELSE(lpos, rpos)
        | "end" => END(lpos, rpos)
        | "false" => FALSE(lpos, rpos)
        | "fn" => FN(lpos, rpos)
        | "rec" => REC(lpos, rpos)
        | "fun" => FUN(lpos, rpos)
        | "hd" => HEAD(lpos, rpos)
        | "if" => IF(lpos, rpos)
        | "Int" => TINT(lpos, rpos)
        | "ise" => ISE(lpos, rpos)
        | "match" => MATCH(lpos, rpos)
        | "Nil" => TNIL(lpos, rpos)
        | "print" => PRINT(lpos, rpos)
        | "then" => THEN(lpos, rpos)
        | "tl" => TAIL(lpos, rpos)
        | "true" => TRUE(lpos, rpos)
        | "var" => VAR(lpos, rpos)
        | "with" => WITH(lpos, rpos)
        | "_" => USCORE(lpos, rpos)
        | _ => Name(s, lpos, rpos)

(* Initialize the lexer. *)
fun init() = ()
%%
%header (functor PlcLexerFun(structure Tokens: PlcParser_TOKENS));
alpha = [A-Za-z];
digit=[0-9];
whitespace=[\ \t];
identifier=[a-zA-Z_][a-zA-Z_0-9]*;
%s COMMENT_STATE;

%%
\n => (lineNumber := !lineNumber + 1; lex());
<INITIAL>{whitespace}+ => (lex());
<INITIAL>{digit}+ => (Nat(strToInt(yytext), yypos, yypos));
<INITIAL>{identifier} => (keyword(yytext, yypos, yypos));
<INITIAL>"," => (COMMA(yypos, yypos));
<INITIAL>";" => (SEMIC(yypos, yypos));
<INITIAL>":" => (COLON(yypos, yypos));
<INITIAL>"::" => (DCOLON(yypos, yypos));
<INITIAL>"=" => (EQ(yypos, yypos));
<INITIAL>"!=" => (NEQ(yypos, yypos));
<INITIAL>"!" => (NOT(yypos, yypos));
<INITIAL>"&&" => (AND(yypos, yypos));
<INITIAL>"<" => (LT(yypos, yypos));
<INITIAL>"<=" => (LTE(yypos, yypos));
<INITIAL>"+" => (PLUS(yypos, yypos));
<INITIAL>"-" => (MINUS(yypos, yypos));
<INITIAL>"*" => (MULTI(yypos, yypos));
<INITIAL>"/" => (DIV(yypos, yypos));
<INITIAL>"[" => (LBRA(yypos, yypos));
<INITIAL>"]" => (RBRA(yypos, yypos));
<INITIAL>"{" => (LKEY(yypos, yypos));
<INITIAL>"}" => (RKEY(yypos, yypos));
<INITIAL>"(" => (LPAR(yypos, yypos));
<INITIAL>")" => (RPAR(yypos, yypos));
<INITIAL>"|" => (PIPE(yypos, yypos));
<INITIAL>"->" => (ARROW(yypos, yypos));
<INITIAL>"=>" => (DARROW(yypos, yypos));
<INITIAL>"_" => (USCORE(yypos, yypos));
<INITIAL>"(*" => (YYBEGIN COMMENT_STATE; lex());
<COMMENT_STATE>"*)" => (YYBEGIN INITIAL; lex());
<COMMENT_STATE>. => (lex());
<INITIAL>. => (error("\n*** Lexer error: bad character ***\n");
      raise Fail("Lexer error: bad character " ^ yytext));