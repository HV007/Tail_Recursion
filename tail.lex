structure Tokens = Tokens
  
  type pos = int
  type svalue = Tokens.svalue
  type ('a,'b) token = ('a,'b) Tokens.token  
  type lexresult = (svalue, pos) token

  val pos = ref 0
  val col = ref 0
  val eof = fn () => Tokens.EOF(!pos, !col)
  val error = fn (str, pos, col) => TextIO.output(TextIO.stdOut, "Unknown Token:" ^ Int.toString(pos) ^ ":" ^ Int.toString(col) ^ ":" ^ str ^ "\n")


  val keywords =
  [
   ("fun", Tokens.FUN),
   ("datatype", Tokens.DATATYPE),
   ("and", Tokens.AND),
   ("of", Tokens.OF),
   ("not", Tokens.NOT),
   ("andalso", Tokens.ANDALSO),
   ("orelse", Tokens.ORELSE),
   ("if", Tokens.IF),
   ("then", Tokens.THEN),
   ("else", Tokens.ELSE)
  ]

  fun findKeywords(str: string, pos: pos, col: pos) =
  case List.find (fn (s, _) => s = str )  keywords of 
  SOME (_, tk) => tk(pos, col) 
  | NONE => if str = "true" then Tokens.CONST(true, pos, col) else if str = "false" then Tokens.CONST(false, pos, col) else Tokens.ID(str, pos, col)
  %%
%header (functor TailLexFun(structure Tokens:Tail_TOKENS));

digit=[0-9];
ws = [\ \t];
%%
[\n|\r\n]       => (pos := (!pos) + 1; col := 0; lex());
{ws}+    => (col := !col + size yytext; lex());
{digit}+ => (col := !col + size yytext; Tokens.NUM
	     (List.foldl (fn (a,r) => ord(a) - ord(#"0") + 10*r) 0 (explode yytext), !pos, !col));
"*"      => (col := !col + size yytext; Tokens.TIMES(!pos,!col));
"-"      => (col := !col + size yytext; Tokens.SUB(!pos,!col));
"+"      => (col := !col + size yytext; Tokens.ADD(!pos,!col));
"/"      => (col := !col + size yytext; Tokens.DIV(!pos,!col));
"^"      => (col := !col + size yytext; Tokens.XOR(!pos,!col));
"="      => (col := !col + size yytext; Tokens.ASSIGN(!pos,!col));
"~"      => (col := !col + size yytext; Tokens.NEGATE(!pos,!col));
"("      => (col := !col + size yytext; Tokens.LPAREN(!pos,!col));
")"      => (col := !col + size yytext; Tokens.RPAREN(!pos,!col));
"\| "     => (col := !col + size yytext; Tokens.BAR(!pos,!col));
","      => (col := !col + size yytext; Tokens.COMMA(!pos,!col));
":"      => (col := !col + size yytext; Tokens.COLON(!pos,!col));
[A-Za-z][A-Za-z0-9_]* => (col := !col + size yytext; findKeywords(yytext,!pos,!col));
.      => (error(yytext, !pos, !col); col := !col + size yytext; lex());