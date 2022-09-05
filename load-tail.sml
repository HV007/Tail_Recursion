open AST
structure TailLrVals = TailLrValsFun(structure Token = LrParser.Token)
structure TailLex = TailLexFun(structure Tokens = TailLrVals.Tokens);
structure TailParser =
	  Join(structure LrParser = LrParser
     	       structure ParserData = TailLrVals.ParserData
     	       structure Lex = TailLex)
     
fun invoke lexstream =
		let fun print_error (s, pos, col) =
		    	TextIO.output(TextIO.stdOut, "Syntax Error: " ^ (Int.toString pos) ^ ":" ^ (Int.toString col) ^ ":" ^ s ^ "\n")
		in
		    TailParser.parse(0,lexstream,print_error,())
		end

fun stringToLexer str =
    let val done = ref false
    	val lexer =  TailParser.makeLexer (fn _ => if (!done) then "" else (done:=true;str))
    in
	lexer
    end	
		
fun parse (lexer) =
    let val dummyEOF = TailLrVals.Tokens.EOF(0,0)
    	val (result, lexer) = invoke lexer
	val (nextToken, lexer) = TailParser.Stream.get lexer
    in
        if TailParser.sameToken(nextToken, dummyEOF) then result
 	else (TextIO.output(TextIO.stdOut, "Warning: Unconsumed input \n"); result)
    end

val parseString = parse o stringToLexer

fun parseFile (fileIn, fileOut) =
let
	val In = TextIO.openIn fileIn
	val str = TextIO.inputAll(In)
	val Out = TextIO.openOut fileOut
in
	TextIO.output(Out, transformProgram(parseString str)); TextIO.closeOut Out; TextIO.closeIn In 
end

val args = CommandLine.arguments()
val fileIn = List.nth(args, 0)
val fileOut = List.nth(args, 1)

fun main() = (parseFile(fileIn, fileOut))

val _ = main()