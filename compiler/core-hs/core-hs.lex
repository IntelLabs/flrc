(* The Intel P to C/Pillar Compiler *)
(* Copyright (C) Intel Corporation, October 2006 *)

structure Tokens = Tokens
structure TextIO = Pervasive.TextIO

type pos = int
type svalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult= (svalue,pos) token

val pos = ref 1
fun eof () = Tokens.EOF(!pos,!pos)
fun error (e,l : int,_) = TextIO.output (TextIO.stdOut, String.concat[
	"line ", (Int.toString l), ": ", e, "\n"
      ])

(* support for incremental construction of strings *)
val sbuf : string list ref = ref []
fun addStr s = sbuf := s :: !sbuf
fun fromNum base (s, i, j) = 
    let
      val s = String.substring2 (s, { start = i, finish = j })
      val c = String.fold (s, 0, fn (c, v) => v * base + Char.toHexDigit c)
    in c
    end
val fromHex = fromNum 16
val fromDec = fromNum 10
fun addChr s = addStr (String.fromChar (Char.fromInt (fromHex (s, 2, String.length s))))
fun finishString () = (Tokens.STRING(String.concat(List.rev(!sbuf)), !pos, !pos) before sbuf := [])
fun lname s = 
    if s = "z7eU" orelse s = "z7eUzh" then Tokens.Z7EU(s, !pos, !pos)
    else if s = "zt" then Tokens.ZT(s, !pos, !pos)
    else Tokens.LNAME(s, !pos, !pos)

%%
%s S;
%header (functor CoreHsLexFun(structure Tokens: CoreHsYacc_TOKENS));
lower = [a-z_];
upper = [A-Z];
digit = [0-9];
xdigit = {digit}|[a-fA-F];
ws    = [\ \t];
namechar = [a-zA-Z0-9_];
charlit = \'.\';
octchar = \'\\{digit}+\';
hexchar = \'\\x{xdigit}+\';
%%


\n       => (pos := (!pos) + 1; lex());
<INITIAL>{ws}+    => (lex());
<INITIAL>{digit}+ => (Tokens.NUM (valOf (IntInf.fromString yytext), !pos, !pos));
<INITIAL>{lower}{namechar}* => (lname(yytext));
<INITIAL>{upper}{namechar}* => (Tokens.UNAME(yytext,!pos,!pos));

<INITIAL>"%module" => (Tokens.MODULE(!pos,!pos));
<INITIAL>"%data" => (Tokens.DATA(!pos,!pos));
<INITIAL>"%newtype" => (Tokens.NEWTYPE(!pos,!pos));
<INITIAL>"%rec" => (Tokens.REC(!pos,!pos));
<INITIAL>"%let" => (Tokens.LET(!pos,!pos));
<INITIAL>"%in" => (Tokens.IN(!pos,!pos));
<INITIAL>"%case" => (Tokens.CASE(!pos,!pos));
<INITIAL>"%of" => (Tokens.OF(!pos,!pos));
<INITIAL>"%cast" => (Tokens.CAST(!pos,!pos));
<INITIAL>"%note" => (Tokens.NOTE(!pos,!pos));
<INITIAL>"%external" => (Tokens.EXTERNAL(!pos,!pos));
<INITIAL>"%dynexternal" => (Tokens.DYNEXTERNAL(!pos,!pos));
<INITIAL>"%label" => (Tokens.LABEL(!pos,!pos));
<INITIAL>"%_" => (Tokens.DEFAULT(!pos,!pos));
<INITIAL>"%trans" => (Tokens.TRANS(!pos,!pos));
<INITIAL>"%sym" => (Tokens.SYM(!pos,!pos));
<INITIAL>"%unsafe" => (Tokens.UNSAFE(!pos,!pos));
<INITIAL>"%left" => (Tokens.LEFT(!pos,!pos));
<INITIAL>"%right" => (Tokens.RIGHT(!pos,!pos));
<INITIAL>"%inst" => (Tokens.INST(!pos,!pos));
<INITIAL>"%nth" => (Tokens.NTH(!pos,!pos));
<INITIAL>"%forall" => (Tokens.FORALL(!pos,!pos));
<INITIAL>";"      => (Tokens.SEMI(!pos,!pos));
<INITIAL>"="      => (Tokens.EQ(!pos,!pos));
<INITIAL>"{"      => (Tokens.LB(!pos,!pos));
<INITIAL>"}"      => (Tokens.RB(!pos,!pos));
<INITIAL>"("      => (Tokens.LP(!pos,!pos));
<INITIAL>")"      => (Tokens.RP(!pos,!pos));
<INITIAL>"@"      => (Tokens.AT(!pos,!pos));
<INITIAL>"\\"     => (Tokens.SLASH(!pos,!pos));
<INITIAL>"->"     => (Tokens.TO(!pos,!pos));
<INITIAL>"::"     => (Tokens.DCOLON(!pos,!pos));
<INITIAL>":=:"    => (Tokens.KEQ(!pos,!pos));
<INITIAL>":"      => (Tokens.COLON(!pos,!pos));
<INITIAL>"*"      => (Tokens.TIMES(!pos,!pos));
<INITIAL>"#"      => (Tokens.HASH(!pos,!pos));
<INITIAL>"?"      => (Tokens.QMARK(!pos,!pos));
<INITIAL>"."      => (Tokens.DOT(!pos,!pos));
<INITIAL>"!"      => (Tokens.BANG(!pos,!pos));
<INITIAL>"-"      => (Tokens.MINUS(!pos,!pos));
<INITIAL>"%"      => (Tokens.PERCENT(!pos,!pos));
<INITIAL>{charlit} => (Tokens.CHAR(Char.toInt (String.sub(yytext, 1)), !pos, !pos));
<INITIAL>{hexchar} => (Tokens.CHAR(fromHex (yytext, 3, String.length yytext - 1), !pos, !pos));
<INITIAL>{octchar} => (Tokens.CHAR(fromDec (yytext, 2, String.length yytext - 1), !pos, !pos));
<INITIAL>"\""     => (YYBEGIN S; lex());

<S>"\\x"{xdigit}{2}		=> ( addChr yytext; lex() );
<S>[^\\"]+			=> ( addStr yytext; lex() );
<S>"\""				=> ( YYBEGIN INITIAL; finishString() );
<S>.              => (error ("bad character in string literal " ^ yytext, !pos, !pos); lex());
<INITIAL>.        => (error ("ignoring bad character "^yytext,!pos,!pos); lex());


