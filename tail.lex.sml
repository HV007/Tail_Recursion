(*#line 33.10 "tail.lex"*)functor TailLexFun(structure Tokens:Tail_TOKENS)(*#line 1.1 "tail.lex.sml"*)
=
   struct
    structure UserDeclarations =
      struct
(*#line 1.1 "tail.lex"*)structure Tokens = Tokens
  
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
  (*#line 37.1 "tail.lex.sml"*)
end (* end of user routines *)
exception LexError (* raised if illegal leaf action tried *)
structure Internal =
	struct

datatype yyfinstate = N of int
type statedata = {fin : yyfinstate list, trans: string}
(* transition & final state table *)
val tab = let
val s = [ 
 (0, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (1, 
"\003\003\003\003\003\003\003\003\003\021\024\003\003\023\003\003\
\\003\003\003\003\003\003\003\003\003\003\003\003\003\003\003\003\
\\021\003\003\003\003\003\003\003\020\019\018\017\016\015\003\014\
\\012\012\012\012\012\012\012\012\012\012\011\003\003\010\003\003\
\\003\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\
\\007\007\007\007\007\007\007\007\007\007\007\003\003\003\009\003\
\\003\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\
\\007\007\007\007\007\007\007\007\007\007\007\003\005\003\004\003\
\\003"
),
 (5, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\006\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (7, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\008\008\008\008\008\008\008\008\008\008\000\000\000\000\000\000\
\\000\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\
\\008\008\008\008\008\008\008\008\008\008\008\000\000\000\000\008\
\\000\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\
\\008\008\008\008\008\008\008\008\008\008\008\000\000\000\000\000\
\\000"
),
 (12, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\013\013\013\013\013\013\013\013\013\013\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (21, 
"\000\000\000\000\000\000\000\000\000\022\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\022\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
(0, "")]
fun f x = x 
val s = List.map f (List.rev (tl (List.rev s))) 
exception LexHackingError 
fun look ((j,x)::r, i: int) = if i = j then x else look(r, i) 
  | look ([], i) = raise LexHackingError
fun g {fin=x, trans=i} = {fin=x, trans=look(s,i)} 
in Vector.fromList(List.map g 
[{fin = [], trans = 0},
{fin = [], trans = 1},
{fin = [], trans = 1},
{fin = [(N 37)], trans = 0},
{fin = [(N 21),(N 37)], trans = 0},
{fin = [(N 1),(N 37)], trans = 5},
{fin = [(N 28)], trans = 0},
{fin = [(N 35),(N 37)], trans = 7},
{fin = [(N 35)], trans = 7},
{fin = [(N 17),(N 37)], trans = 0},
{fin = [(N 19),(N 37)], trans = 0},
{fin = [(N 32),(N 37)], trans = 0},
{fin = [(N 7),(N 37)], trans = 12},
{fin = [(N 7)], trans = 12},
{fin = [(N 15),(N 37)], trans = 0},
{fin = [(N 11),(N 37)], trans = 0},
{fin = [(N 30),(N 37)], trans = 0},
{fin = [(N 13),(N 37)], trans = 0},
{fin = [(N 9),(N 37)], trans = 0},
{fin = [(N 25),(N 37)], trans = 0},
{fin = [(N 23),(N 37)], trans = 0},
{fin = [(N 4),(N 37)], trans = 21},
{fin = [(N 4)], trans = 21},
{fin = [(N 1),(N 37)], trans = 0},
{fin = [(N 1)], trans = 0}])
end
structure StartStates =
	struct
	datatype yystartstate = STARTSTATE of int

(* start state definitions *)

val INITIAL = STARTSTATE 1;

end
type result = UserDeclarations.lexresult
	exception LexerError (* raised if illegal leaf action tried *)
end

structure YYPosInt : INTEGER = Int
fun makeLexer yyinput =
let	val yygone0= YYPosInt.fromInt ~1
	val yyb = ref "\n" 		(* buffer *)
	val yybl = ref 1		(*buffer length *)
	val yybufpos = ref 1		(* location of next character to use *)
	val yygone = ref yygone0	(* position in file of beginning of buffer *)
	val yydone = ref false		(* eof found yet? *)
	val yybegin = ref 1		(*Current 'start state' for lexer *)

	val YYBEGIN = fn (Internal.StartStates.STARTSTATE x) =>
		 yybegin := x

fun lex () : Internal.result =
let fun continue() = lex() in
  let fun scan (s,AcceptingLeaves : Internal.yyfinstate list list,l,i0) =
	let fun action (i,nil) = raise LexError
	| action (i,nil::l) = action (i-1,l)
	| action (i,(node::acts)::l) =
		case node of
		    Internal.N yyk => 
			(let fun yymktext() = String.substring(!yyb,i0,i-i0)
			     val yypos = YYPosInt.+(YYPosInt.fromInt i0, !yygone)
			open UserDeclarations Internal.StartStates
 in (yybufpos := i; case yyk of 

			(* Application actions *)

  1 => ((*#line 38.21 "tail.lex"*)pos := (!pos) + 1; col := 0; lex()(*#line 189.1 "tail.lex.sml"*)
)
| 11 => let val yytext=yymktext() in (*#line 43.14 "tail.lex"*)col := !col + size yytext; Tokens.SUB(!pos,!col)(*#line 191.1 "tail.lex.sml"*)
 end
| 13 => let val yytext=yymktext() in (*#line 44.14 "tail.lex"*)col := !col + size yytext; Tokens.ADD(!pos,!col)(*#line 193.1 "tail.lex.sml"*)
 end
| 15 => let val yytext=yymktext() in (*#line 45.14 "tail.lex"*)col := !col + size yytext; Tokens.DIV(!pos,!col)(*#line 195.1 "tail.lex.sml"*)
 end
| 17 => let val yytext=yymktext() in (*#line 46.14 "tail.lex"*)col := !col + size yytext; Tokens.XOR(!pos,!col)(*#line 197.1 "tail.lex.sml"*)
 end
| 19 => let val yytext=yymktext() in (*#line 47.14 "tail.lex"*)col := !col + size yytext; Tokens.ASSIGN(!pos,!col)(*#line 199.1 "tail.lex.sml"*)
 end
| 21 => let val yytext=yymktext() in (*#line 48.14 "tail.lex"*)col := !col + size yytext; Tokens.NEGATE(!pos,!col)(*#line 201.1 "tail.lex.sml"*)
 end
| 23 => let val yytext=yymktext() in (*#line 49.14 "tail.lex"*)col := !col + size yytext; Tokens.LPAREN(!pos,!col)(*#line 203.1 "tail.lex.sml"*)
 end
| 25 => let val yytext=yymktext() in (*#line 50.14 "tail.lex"*)col := !col + size yytext; Tokens.RPAREN(!pos,!col)(*#line 205.1 "tail.lex.sml"*)
 end
| 28 => let val yytext=yymktext() in (*#line 51.15 "tail.lex"*)col := !col + size yytext; Tokens.BAR(!pos,!col)(*#line 207.1 "tail.lex.sml"*)
 end
| 30 => let val yytext=yymktext() in (*#line 52.14 "tail.lex"*)col := !col + size yytext; Tokens.COMMA(!pos,!col)(*#line 209.1 "tail.lex.sml"*)
 end
| 32 => let val yytext=yymktext() in (*#line 53.14 "tail.lex"*)col := !col + size yytext; Tokens.COLON(!pos,!col)(*#line 211.1 "tail.lex.sml"*)
 end
| 35 => let val yytext=yymktext() in (*#line 54.27 "tail.lex"*)col := !col + size yytext; findKeywords(yytext,!pos,!col)(*#line 213.1 "tail.lex.sml"*)
 end
| 37 => let val yytext=yymktext() in (*#line 55.12 "tail.lex"*)error(yytext, !pos, !col); col := !col + size yytext; lex()(*#line 215.1 "tail.lex.sml"*)
 end
| 4 => let val yytext=yymktext() in (*#line 39.14 "tail.lex"*)col := !col + size yytext; lex()(*#line 217.1 "tail.lex.sml"*)
 end
| 7 => let val yytext=yymktext() in (*#line 40.14 "tail.lex"*)col := !col + size yytext; Tokens.NUM
	     (List.foldl (fn (a,r) => ord(a) - ord(#"0") + 10*r) 0 (explode yytext), !pos, !col)(*#line 220.1 "tail.lex.sml"*)
 end
| 9 => let val yytext=yymktext() in (*#line 42.14 "tail.lex"*)col := !col + size yytext; Tokens.TIMES(!pos,!col)(*#line 222.1 "tail.lex.sml"*)
 end
| _ => raise Internal.LexerError

		) end )

	val {fin,trans} = Vector.sub(Internal.tab, s)
	val NewAcceptingLeaves = fin::AcceptingLeaves
	in if l = !yybl then
	     if trans = #trans(Vector.sub(Internal.tab,0))
	       then action(l,NewAcceptingLeaves
) else	    let val newchars= if !yydone then "" else yyinput 1024
	    in if (String.size newchars)=0
		  then (yydone := true;
		        if (l=i0) then UserDeclarations.eof ()
		                  else action(l,NewAcceptingLeaves))
		  else (if i0=l then yyb := newchars
		     else yyb := String.substring(!yyb,i0,l-i0)^newchars;
		     yygone := YYPosInt.+(!yygone, YYPosInt.fromInt i0);
		     yybl := String.size (!yyb);
		     scan (s,AcceptingLeaves,l-i0,0))
	    end
	  else let val NewChar = Char.ord(CharVector.sub(!yyb,l))
		val NewChar = if NewChar<128 then NewChar else 128
		val NewState = Char.ord(CharVector.sub(trans,NewChar))
		in if NewState=0 then action(l,NewAcceptingLeaves)
		else scan(NewState,NewAcceptingLeaves,l+1,i0)
	end
	end
(*
	val start= if String.substring(!yyb,!yybufpos-1,1)="\n"
then !yybegin+1 else !yybegin
*)
	in scan(!yybegin (* start *),nil,!yybufpos,!yybufpos)
    end
end
  in lex
  end
end
