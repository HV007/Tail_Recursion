functor TailLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : Tail_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct
(* User  declarations *)
fun lookup "special" = 1000
  | lookup s = 0 


end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\012\000\000\000\
\\001\000\001\000\016\000\000\000\
\\001\000\001\000\025\000\000\000\
\\001\000\001\000\037\000\002\000\036\000\003\000\035\000\012\000\034\000\
\\013\000\033\000\014\000\032\000\022\000\031\000\000\000\
\\001\000\001\000\062\000\000\000\
\\001\000\001\000\076\000\000\000\
\\001\000\004\000\053\000\005\000\052\000\006\000\051\000\007\000\050\000\
\\008\000\049\000\009\000\048\000\010\000\047\000\011\000\046\000\
\\015\000\067\000\024\000\045\000\025\000\044\000\000\000\
\\001\000\004\000\053\000\005\000\052\000\006\000\051\000\007\000\050\000\
\\008\000\049\000\009\000\048\000\010\000\047\000\011\000\046\000\
\\016\000\077\000\024\000\045\000\025\000\044\000\000\000\
\\001\000\004\000\053\000\005\000\052\000\006\000\051\000\007\000\050\000\
\\008\000\049\000\009\000\048\000\010\000\047\000\011\000\046\000\
\\021\000\066\000\024\000\045\000\025\000\044\000\000\000\
\\001\000\017\000\007\000\000\000\
\\001\000\017\000\007\000\018\000\006\000\000\000\
\\001\000\021\000\042\000\027\000\041\000\000\000\
\\001\000\021\000\075\000\027\000\041\000\000\000\
\\001\000\022\000\021\000\000\000\
\\001\000\023\000\000\000\000\000\
\\001\000\025\000\018\000\000\000\
\\001\000\025\000\064\000\000\000\
\\082\000\017\000\007\000\000\000\
\\083\000\017\000\007\000\000\000\
\\084\000\019\000\017\000\000\000\
\\085\000\000\000\
\\086\000\000\000\
\\087\000\026\000\038\000\000\000\
\\088\000\000\000\
\\089\000\000\000\
\\090\000\024\000\072\000\000\000\
\\091\000\020\000\039\000\000\000\
\\092\000\000\000\
\\093\000\000\000\
\\094\000\000\000\
\\095\000\000\000\
\\096\000\019\000\019\000\000\000\
\\097\000\026\000\020\000\000\000\
\\098\000\026\000\020\000\000\000\
\\099\000\000\000\
\\100\000\000\000\
\\101\000\004\000\053\000\005\000\052\000\006\000\051\000\007\000\050\000\
\\008\000\049\000\009\000\048\000\010\000\047\000\011\000\046\000\
\\024\000\045\000\025\000\044\000\000\000\
\\102\000\004\000\053\000\005\000\052\000\006\000\051\000\007\000\050\000\
\\008\000\049\000\009\000\048\000\010\000\047\000\011\000\046\000\
\\024\000\045\000\025\000\044\000\000\000\
\\103\000\004\000\053\000\005\000\052\000\006\000\051\000\007\000\050\000\
\\008\000\049\000\009\000\048\000\010\000\047\000\011\000\046\000\
\\024\000\045\000\025\000\044\000\000\000\
\\104\000\000\000\
\\105\000\000\000\
\\106\000\000\000\
\\107\000\000\000\
\\108\000\000\000\
\\109\000\000\000\
\\110\000\000\000\
\\111\000\000\000\
\\112\000\000\000\
\\113\000\000\000\
\\114\000\000\000\
\\115\000\000\000\
\\116\000\028\000\057\000\000\000\
\\117\000\024\000\072\000\000\000\
\\118\000\028\000\056\000\000\000\
\\119\000\024\000\072\000\000\000\
\\120\000\004\000\053\000\005\000\052\000\006\000\051\000\007\000\050\000\
\\008\000\049\000\009\000\048\000\010\000\047\000\011\000\046\000\
\\024\000\045\000\025\000\044\000\000\000\
\\121\000\004\000\053\000\005\000\052\000\006\000\051\000\007\000\050\000\
\\008\000\049\000\009\000\048\000\010\000\047\000\011\000\046\000\
\\024\000\045\000\025\000\044\000\000\000\
\\122\000\004\000\053\000\005\000\052\000\006\000\051\000\007\000\050\000\
\\008\000\049\000\009\000\048\000\010\000\047\000\011\000\046\000\
\\024\000\045\000\025\000\044\000\000\000\
\\123\000\024\000\072\000\000\000\
\\124\000\028\000\078\000\000\000\
\\125\000\024\000\072\000\000\000\
\\126\000\022\000\059\000\028\000\058\000\000\000\
\\127\000\000\000\
\"
val actionRowNumbers =
"\010\000\009\000\018\000\029\000\
\\000\000\001\000\017\000\030\000\
\\019\000\021\000\015\000\031\000\
\\033\000\035\000\013\000\000\000\
\\002\000\001\000\001\000\003\000\
\\020\000\022\000\024\000\026\000\
\\032\000\034\000\003\000\011\000\
\\038\000\003\000\003\000\050\000\
\\049\000\053\000\051\000\061\000\
\\002\000\004\000\056\000\003\000\
\\016\000\003\000\043\000\041\000\
\\048\000\047\000\046\000\045\000\
\\044\000\042\000\040\000\039\000\
\\008\000\006\000\004\000\004\000\
\\004\000\003\000\023\000\025\000\
\\028\000\037\000\003\000\055\000\
\\062\000\003\000\054\000\052\000\
\\060\000\012\000\005\000\036\000\
\\007\000\059\000\027\000\003\000\
\\004\000\057\000\058\000\014\000"
val gotoT =
"\
\\006\000\003\000\007\000\002\000\013\000\001\000\014\000\079\000\000\000\
\\006\000\003\000\007\000\006\000\000\000\
\\006\000\007\000\000\000\
\\000\000\
\\011\000\009\000\012\000\008\000\000\000\
\\003\000\013\000\004\000\012\000\005\000\011\000\000\000\
\\006\000\007\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\011\000\020\000\000\000\
\\009\000\022\000\010\000\021\000\000\000\
\\003\000\013\000\004\000\024\000\000\000\
\\003\000\025\000\000\000\
\\001\000\028\000\002\000\027\000\016\000\026\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\038\000\016\000\026\000\000\000\
\\000\000\
\\015\000\041\000\000\000\
\\001\000\052\000\016\000\026\000\000\000\
\\001\000\053\000\016\000\026\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\009\000\058\000\000\000\
\\008\000\059\000\000\000\
\\015\000\041\000\000\000\
\\001\000\061\000\016\000\026\000\000\000\
\\000\000\
\\001\000\063\000\016\000\026\000\000\000\
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
\\015\000\041\000\000\000\
\\015\000\041\000\000\000\
\\008\000\066\000\000\000\
\\008\000\067\000\000\000\
\\008\000\068\000\000\000\
\\001\000\028\000\002\000\069\000\016\000\026\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\015\000\041\000\000\000\
\\001\000\071\000\016\000\026\000\000\000\
\\015\000\041\000\000\000\
\\000\000\
\\001\000\072\000\016\000\026\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\015\000\041\000\000\000\
\\015\000\041\000\000\000\
\\000\000\
\\000\000\
\\001\000\077\000\016\000\026\000\000\000\
\\008\000\078\000\000\000\
\\015\000\041\000\000\000\
\\000\000\
\\000\000\
\"
val numstates = 80
val numrules = 46
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
 | CONST of unit ->  (bool) | NUM of unit ->  (int)
 | ID of unit ->  (string) | UNOP of unit ->  (AST.unop)
 | BINOP of unit ->  (AST.binop) | PROGRAM of unit ->  (AST.Program)
 | DATATYPEDECLARATION of unit ->  (AST.Datatype list)
 | DATATYPEDEFINITIONLIST of unit ->  (AST.Datatype list)
 | DATATYPEDEFINITION of unit ->  (AST.Datatype)
 | CONSTRUCTORLIST of unit ->  (AST.ConstructorDef list)
 | CONSTRUCTOR of unit ->  (AST.ConstructorDef)
 | TYPELIST of unit ->  (string list)
 | FUNCTIONDECLARATIONLIST of unit ->  ( ( (AST.Rewrite list) list )  list)
 | FUNCTIONDECLARATION of unit ->  ( ( AST.Rewrite list )  list)
 | FUNCTIONLIST of unit ->  ( ( AST.Rewrite list )  list)
 | FUNCTION of unit ->  (AST.Rewrite list)
 | REWRITE of unit ->  (AST.Rewrite)
 | ARGUMENTLIST of unit ->  ( ( AST.exp list ) )
 | EXPRESSION of unit ->  (AST.exp)
end
type svalue = MlyValue.svalue
type result = AST.Program
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
fn (T 22) => true | _ => false
val showTerminal =
fn (T 0) => "ID"
  | (T 1) => "NUM"
  | (T 2) => "CONST"
  | (T 3) => "ADD"
  | (T 4) => "SUB"
  | (T 5) => "DIV"
  | (T 6) => "LESSTHAN"
  | (T 7) => "GREATERTHAN"
  | (T 8) => "ANDALSO"
  | (T 9) => "ORELSE"
  | (T 10) => "XOR"
  | (T 11) => "NEGATE"
  | (T 12) => "NOT"
  | (T 13) => "IF"
  | (T 14) => "THEN"
  | (T 15) => "ELSE"
  | (T 16) => "FUN"
  | (T 17) => "DATATYPE"
  | (T 18) => "AND"
  | (T 19) => "OF"
  | (T 20) => "RPAREN"
  | (T 21) => "LPAREN"
  | (T 22) => "EOF"
  | (T 23) => "TIMES"
  | (T 24) => "ASSIGN"
  | (T 25) => "BAR"
  | (T 26) => "COMMA"
  | (T 27) => "COLON"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 27) $$ (T 26) $$ (T 25) $$ (T 24) $$ (T 23) $$ (T 22) $$ (T 21)
 $$ (T 20) $$ (T 19) $$ (T 18) $$ (T 17) $$ (T 16) $$ (T 15) $$ (T 14)
 $$ (T 13) $$ (T 12) $$ (T 11) $$ (T 10) $$ (T 9) $$ (T 8) $$ (T 7)
 $$ (T 6) $$ (T 5) $$ (T 4) $$ (T 3)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.FUNCTIONDECLARATIONLIST 
FUNCTIONDECLARATIONLIST1, _, FUNCTIONDECLARATIONLIST1right)) :: ( _, (
 MlyValue.DATATYPEDECLARATION DATATYPEDECLARATION1, 
DATATYPEDECLARATION1left, _)) :: rest671)) => let val  result = 
MlyValue.PROGRAM (fn _ => let val  (DATATYPEDECLARATION as 
DATATYPEDECLARATION1) = DATATYPEDECLARATION1 ()
 val  (FUNCTIONDECLARATIONLIST as FUNCTIONDECLARATIONLIST1) = 
FUNCTIONDECLARATIONLIST1 ()
 in (AST.ProgramDef(DATATYPEDECLARATION, FUNCTIONDECLARATIONLIST))
end
)
 in ( LrTable.NT 13, ( result, DATATYPEDECLARATION1left, 
FUNCTIONDECLARATIONLIST1right), rest671)
end
|  ( 1, ( ( _, ( MlyValue.FUNCTIONDECLARATIONLIST 
FUNCTIONDECLARATIONLIST1, FUNCTIONDECLARATIONLIST1left, 
FUNCTIONDECLARATIONLIST1right)) :: rest671)) => let val  result = 
MlyValue.PROGRAM (fn _ => let val  (FUNCTIONDECLARATIONLIST as 
FUNCTIONDECLARATIONLIST1) = FUNCTIONDECLARATIONLIST1 ()
 in (AST.ProgramDef([], FUNCTIONDECLARATIONLIST))
end)
 in ( LrTable.NT 13, ( result, FUNCTIONDECLARATIONLIST1left, 
FUNCTIONDECLARATIONLIST1right), rest671)
end
|  ( 2, ( ( _, ( MlyValue.DATATYPEDEFINITIONLIST 
DATATYPEDEFINITIONLIST1, _, DATATYPEDEFINITIONLIST1right)) :: ( _, ( _
, DATATYPE1left, _)) :: rest671)) => let val  result = 
MlyValue.DATATYPEDECLARATION (fn _ => let val  (DATATYPEDEFINITIONLIST
 as DATATYPEDEFINITIONLIST1) = DATATYPEDEFINITIONLIST1 ()
 in (DATATYPEDEFINITIONLIST)
end)
 in ( LrTable.NT 12, ( result, DATATYPE1left, 
DATATYPEDEFINITIONLIST1right), rest671)
end
|  ( 3, ( ( _, ( MlyValue.DATATYPEDEFINITION DATATYPEDEFINITION1, _, 
DATATYPEDEFINITION1right)) :: _ :: ( _, ( 
MlyValue.DATATYPEDEFINITIONLIST DATATYPEDEFINITIONLIST1, 
DATATYPEDEFINITIONLIST1left, _)) :: rest671)) => let val  result = 
MlyValue.DATATYPEDEFINITIONLIST (fn _ => let val  (
DATATYPEDEFINITIONLIST as DATATYPEDEFINITIONLIST1) = 
DATATYPEDEFINITIONLIST1 ()
 val  (DATATYPEDEFINITION as DATATYPEDEFINITION1) = 
DATATYPEDEFINITION1 ()
 in (DATATYPEDEFINITIONLIST @ [DATATYPEDEFINITION])
end)
 in ( LrTable.NT 11, ( result, DATATYPEDEFINITIONLIST1left, 
DATATYPEDEFINITION1right), rest671)
end
|  ( 4, ( ( _, ( MlyValue.DATATYPEDEFINITION DATATYPEDEFINITION1, 
DATATYPEDEFINITION1left, DATATYPEDEFINITION1right)) :: rest671)) =>
 let val  result = MlyValue.DATATYPEDEFINITIONLIST (fn _ => let val  (
DATATYPEDEFINITION as DATATYPEDEFINITION1) = DATATYPEDEFINITION1 ()
 in ([DATATYPEDEFINITION])
end)
 in ( LrTable.NT 11, ( result, DATATYPEDEFINITION1left, 
DATATYPEDEFINITION1right), rest671)
end
|  ( 5, ( ( _, ( MlyValue.CONSTRUCTORLIST CONSTRUCTORLIST1, _, 
CONSTRUCTORLIST1right)) :: _ :: ( _, ( MlyValue.ID ID1, ID1left, _))
 :: rest671)) => let val  result = MlyValue.DATATYPEDEFINITION (fn _
 => let val  (ID as ID1) = ID1 ()
 val  (CONSTRUCTORLIST as CONSTRUCTORLIST1) = CONSTRUCTORLIST1 ()
 in (AST.DatatypeDef(ID, CONSTRUCTORLIST))
end)
 in ( LrTable.NT 10, ( result, ID1left, CONSTRUCTORLIST1right), 
rest671)
end
|  ( 6, ( ( _, ( MlyValue.CONSTRUCTOR CONSTRUCTOR1, _, 
CONSTRUCTOR1right)) :: _ :: ( _, ( MlyValue.CONSTRUCTORLIST 
CONSTRUCTORLIST1, CONSTRUCTORLIST1left, _)) :: rest671)) => let val  
result = MlyValue.CONSTRUCTORLIST (fn _ => let val  (CONSTRUCTORLIST
 as CONSTRUCTORLIST1) = CONSTRUCTORLIST1 ()
 val  (CONSTRUCTOR as CONSTRUCTOR1) = CONSTRUCTOR1 ()
 in (CONSTRUCTORLIST @ [CONSTRUCTOR])
end)
 in ( LrTable.NT 9, ( result, CONSTRUCTORLIST1left, CONSTRUCTOR1right)
, rest671)
end
|  ( 7, ( ( _, ( MlyValue.CONSTRUCTOR CONSTRUCTOR1, CONSTRUCTOR1left, 
CONSTRUCTOR1right)) :: rest671)) => let val  result = 
MlyValue.CONSTRUCTORLIST (fn _ => let val  (CONSTRUCTOR as 
CONSTRUCTOR1) = CONSTRUCTOR1 ()
 in ([CONSTRUCTOR])
end)
 in ( LrTable.NT 9, ( result, CONSTRUCTOR1left, CONSTRUCTOR1right), 
rest671)
end
|  ( 8, ( ( _, ( MlyValue.TYPELIST TYPELIST1, _, TYPELIST1right)) :: _
 :: ( _, ( MlyValue.ID ID1, ID1left, _)) :: rest671)) => let val  
result = MlyValue.CONSTRUCTOR (fn _ => let val  (ID as ID1) = ID1 ()
 val  (TYPELIST as TYPELIST1) = TYPELIST1 ()
 in (AST.AppDef(ID, TYPELIST))
end)
 in ( LrTable.NT 8, ( result, ID1left, TYPELIST1right), rest671)
end
|  ( 9, ( ( _, ( MlyValue.ID ID1, ID1left, ID1right)) :: rest671)) =>
 let val  result = MlyValue.CONSTRUCTOR (fn _ => let val  (ID as ID1)
 = ID1 ()
 in (AST.BaseDef(ID))
end)
 in ( LrTable.NT 8, ( result, ID1left, ID1right), rest671)
end
|  ( 10, ( ( _, ( MlyValue.ID ID1, _, ID1right)) :: _ :: ( _, ( 
MlyValue.TYPELIST TYPELIST1, TYPELIST1left, _)) :: rest671)) => let
 val  result = MlyValue.TYPELIST (fn _ => let val  (TYPELIST as 
TYPELIST1) = TYPELIST1 ()
 val  (ID as ID1) = ID1 ()
 in (TYPELIST @ [ID])
end)
 in ( LrTable.NT 7, ( result, TYPELIST1left, ID1right), rest671)
end
|  ( 11, ( ( _, ( MlyValue.ID ID1, ID1left, ID1right)) :: rest671)) =>
 let val  result = MlyValue.TYPELIST (fn _ => let val  (ID as ID1) = 
ID1 ()
 in ([ID])
end)
 in ( LrTable.NT 7, ( result, ID1left, ID1right), rest671)
end
|  ( 12, ( ( _, ( MlyValue.FUNCTIONDECLARATION FUNCTIONDECLARATION1, 
FUNCTIONDECLARATION1left, FUNCTIONDECLARATION1right)) :: rest671)) =>
 let val  result = MlyValue.FUNCTIONDECLARATIONLIST (fn _ => let val 
 (FUNCTIONDECLARATION as FUNCTIONDECLARATION1) = FUNCTIONDECLARATION1
 ()
 in ([FUNCTIONDECLARATION])
end)
 in ( LrTable.NT 6, ( result, FUNCTIONDECLARATION1left, 
FUNCTIONDECLARATION1right), rest671)
end
|  ( 13, ( ( _, ( MlyValue.FUNCTIONDECLARATION FUNCTIONDECLARATION1, _
, FUNCTIONDECLARATION1right)) :: ( _, ( 
MlyValue.FUNCTIONDECLARATIONLIST FUNCTIONDECLARATIONLIST1, 
FUNCTIONDECLARATIONLIST1left, _)) :: rest671)) => let val  result = 
MlyValue.FUNCTIONDECLARATIONLIST (fn _ => let val  (
FUNCTIONDECLARATIONLIST as FUNCTIONDECLARATIONLIST1) = 
FUNCTIONDECLARATIONLIST1 ()
 val  (FUNCTIONDECLARATION as FUNCTIONDECLARATION1) = 
FUNCTIONDECLARATION1 ()
 in (FUNCTIONDECLARATIONLIST @ [FUNCTIONDECLARATION])
end)
 in ( LrTable.NT 6, ( result, FUNCTIONDECLARATIONLIST1left, 
FUNCTIONDECLARATION1right), rest671)
end
|  ( 14, ( ( _, ( MlyValue.FUNCTIONLIST FUNCTIONLIST1, _, 
FUNCTIONLIST1right)) :: ( _, ( _, FUN1left, _)) :: rest671)) => let
 val  result = MlyValue.FUNCTIONDECLARATION (fn _ => let val  (
FUNCTIONLIST as FUNCTIONLIST1) = FUNCTIONLIST1 ()
 in (FUNCTIONLIST)
end)
 in ( LrTable.NT 5, ( result, FUN1left, FUNCTIONLIST1right), rest671)

end
|  ( 15, ( ( _, ( MlyValue.FUNCTION FUNCTION1, _, FUNCTION1right)) ::
 _ :: ( _, ( MlyValue.FUNCTIONLIST FUNCTIONLIST1, FUNCTIONLIST1left, _
)) :: rest671)) => let val  result = MlyValue.FUNCTIONLIST (fn _ =>
 let val  (FUNCTIONLIST as FUNCTIONLIST1) = FUNCTIONLIST1 ()
 val  (FUNCTION as FUNCTION1) = FUNCTION1 ()
 in (FUNCTIONLIST @ [FUNCTION])
end)
 in ( LrTable.NT 4, ( result, FUNCTIONLIST1left, FUNCTION1right), 
rest671)
end
|  ( 16, ( ( _, ( MlyValue.FUNCTION FUNCTION1, FUNCTION1left, 
FUNCTION1right)) :: rest671)) => let val  result = 
MlyValue.FUNCTIONLIST (fn _ => let val  (FUNCTION as FUNCTION1) = 
FUNCTION1 ()
 in ([FUNCTION])
end)
 in ( LrTable.NT 4, ( result, FUNCTION1left, FUNCTION1right), rest671)

end
|  ( 17, ( ( _, ( MlyValue.REWRITE REWRITE1, _, REWRITE1right)) :: _
 :: ( _, ( MlyValue.FUNCTION FUNCTION1, FUNCTION1left, _)) :: rest671)
) => let val  result = MlyValue.FUNCTION (fn _ => let val  (FUNCTION
 as FUNCTION1) = FUNCTION1 ()
 val  (REWRITE as REWRITE1) = REWRITE1 ()
 in (FUNCTION @ [REWRITE])
end)
 in ( LrTable.NT 3, ( result, FUNCTION1left, REWRITE1right), rest671)

end
|  ( 18, ( ( _, ( MlyValue.REWRITE REWRITE1, REWRITE1left, 
REWRITE1right)) :: rest671)) => let val  result = MlyValue.FUNCTION
 (fn _ => let val  (REWRITE as REWRITE1) = REWRITE1 ()
 in ([REWRITE])
end)
 in ( LrTable.NT 3, ( result, REWRITE1left, REWRITE1right), rest671)

end
|  ( 19, ( ( _, ( MlyValue.EXPRESSION EXPRESSION1, _, EXPRESSION1right
)) :: _ :: _ :: ( _, ( MlyValue.ARGUMENTLIST ARGUMENTLIST1, _, _)) ::
 _ :: ( _, ( MlyValue.ID ID1, ID1left, _)) :: rest671)) => let val  
result = MlyValue.REWRITE (fn _ => let val  (ID as ID1) = ID1 ()
 val  (ARGUMENTLIST as ARGUMENTLIST1) = ARGUMENTLIST1 ()
 val  (EXPRESSION as EXPRESSION1) = EXPRESSION1 ()
 in (AST.RewriteRule(ID, ARGUMENTLIST, EXPRESSION))
end)
 in ( LrTable.NT 2, ( result, ID1left, EXPRESSION1right), rest671)
end
|  ( 20, ( ( _, ( MlyValue.EXPRESSION EXPRESSION1, _, EXPRESSION1right
)) :: _ :: ( _, ( MlyValue.ARGUMENTLIST ARGUMENTLIST1, 
ARGUMENTLIST1left, _)) :: rest671)) => let val  result = 
MlyValue.ARGUMENTLIST (fn _ => let val  (ARGUMENTLIST as ARGUMENTLIST1
) = ARGUMENTLIST1 ()
 val  (EXPRESSION as EXPRESSION1) = EXPRESSION1 ()
 in (ARGUMENTLIST @ [EXPRESSION])
end)
 in ( LrTable.NT 1, ( result, ARGUMENTLIST1left, EXPRESSION1right), 
rest671)
end
|  ( 21, ( ( _, ( MlyValue.EXPRESSION EXPRESSION1, EXPRESSION1left, 
EXPRESSION1right)) :: rest671)) => let val  result = 
MlyValue.ARGUMENTLIST (fn _ => let val  (EXPRESSION as EXPRESSION1) = 
EXPRESSION1 ()
 in ([EXPRESSION])
end)
 in ( LrTable.NT 1, ( result, EXPRESSION1left, EXPRESSION1right), 
rest671)
end
|  ( 22, ( ( _, ( _, ADD1left, ADD1right)) :: rest671)) => let val  
result = MlyValue.BINOP (fn _ => (AST.Add))
 in ( LrTable.NT 14, ( result, ADD1left, ADD1right), rest671)
end
|  ( 23, ( ( _, ( _, SUB1left, SUB1right)) :: rest671)) => let val  
result = MlyValue.BINOP (fn _ => (AST.Sub))
 in ( LrTable.NT 14, ( result, SUB1left, SUB1right), rest671)
end
|  ( 24, ( ( _, ( _, TIMES1left, TIMES1right)) :: rest671)) => let
 val  result = MlyValue.BINOP (fn _ => (AST.Mul))
 in ( LrTable.NT 14, ( result, TIMES1left, TIMES1right), rest671)
end
|  ( 25, ( ( _, ( _, DIV1left, DIV1right)) :: rest671)) => let val  
result = MlyValue.BINOP (fn _ => (AST.Div))
 in ( LrTable.NT 14, ( result, DIV1left, DIV1right), rest671)
end
|  ( 26, ( ( _, ( _, ASSIGN1left, ASSIGN1right)) :: rest671)) => let
 val  result = MlyValue.BINOP (fn _ => (AST.Equals))
 in ( LrTable.NT 14, ( result, ASSIGN1left, ASSIGN1right), rest671)

end
|  ( 27, ( ( _, ( _, LESSTHAN1left, LESSTHAN1right)) :: rest671)) =>
 let val  result = MlyValue.BINOP (fn _ => (AST.LessThan))
 in ( LrTable.NT 14, ( result, LESSTHAN1left, LESSTHAN1right), rest671
)
end
|  ( 28, ( ( _, ( _, GREATERTHAN1left, GREATERTHAN1right)) :: rest671)
) => let val  result = MlyValue.BINOP (fn _ => (AST.GreaterThan))
 in ( LrTable.NT 14, ( result, GREATERTHAN1left, GREATERTHAN1right), 
rest671)
end
|  ( 29, ( ( _, ( _, ANDALSO1left, ANDALSO1right)) :: rest671)) => let
 val  result = MlyValue.BINOP (fn _ => (AST.And))
 in ( LrTable.NT 14, ( result, ANDALSO1left, ANDALSO1right), rest671)

end
|  ( 30, ( ( _, ( _, ORELSE1left, ORELSE1right)) :: rest671)) => let
 val  result = MlyValue.BINOP (fn _ => (AST.Or))
 in ( LrTable.NT 14, ( result, ORELSE1left, ORELSE1right), rest671)

end
|  ( 31, ( ( _, ( _, XOR1left, XOR1right)) :: rest671)) => let val  
result = MlyValue.BINOP (fn _ => (AST.Xor))
 in ( LrTable.NT 14, ( result, XOR1left, XOR1right), rest671)
end
|  ( 32, ( ( _, ( _, NEGATE1left, NEGATE1right)) :: rest671)) => let
 val  result = MlyValue.UNOP (fn _ => (AST.Negate))
 in ( LrTable.NT 15, ( result, NEGATE1left, NEGATE1right), rest671)

end
|  ( 33, ( ( _, ( _, NOT1left, NOT1right)) :: rest671)) => let val  
result = MlyValue.UNOP (fn _ => (AST.Not))
 in ( LrTable.NT 15, ( result, NOT1left, NOT1right), rest671)
end
|  ( 34, ( ( _, ( MlyValue.NUM NUM1, NUM1left, NUM1right)) :: rest671)
) => let val  result = MlyValue.EXPRESSION (fn _ => let val  (NUM as 
NUM1) = NUM1 ()
 in (AST.NumExp(NUM, ["UNDEFINED"]))
end)
 in ( LrTable.NT 0, ( result, NUM1left, NUM1right), rest671)
end
|  ( 35, ( ( _, ( MlyValue.TYPELIST TYPELIST1, _, TYPELIST1right)) ::
 _ :: ( _, ( MlyValue.NUM NUM1, NUM1left, _)) :: rest671)) => let val 
 result = MlyValue.EXPRESSION (fn _ => let val  (NUM as NUM1) = NUM1
 ()
 val  (TYPELIST as TYPELIST1) = TYPELIST1 ()
 in (AST.NumExp(NUM, TYPELIST))
end)
 in ( LrTable.NT 0, ( result, NUM1left, TYPELIST1right), rest671)
end
|  ( 36, ( ( _, ( MlyValue.CONST CONST1, CONST1left, CONST1right)) :: 
rest671)) => let val  result = MlyValue.EXPRESSION (fn _ => let val  (
CONST as CONST1) = CONST1 ()
 in (AST.BoolExp(CONST, ["UNDEFINED"]))
end)
 in ( LrTable.NT 0, ( result, CONST1left, CONST1right), rest671)
end
|  ( 37, ( ( _, ( MlyValue.TYPELIST TYPELIST1, _, TYPELIST1right)) ::
 _ :: ( _, ( MlyValue.CONST CONST1, CONST1left, _)) :: rest671)) =>
 let val  result = MlyValue.EXPRESSION (fn _ => let val  (CONST as 
CONST1) = CONST1 ()
 val  (TYPELIST as TYPELIST1) = TYPELIST1 ()
 in (AST.BoolExp(CONST, TYPELIST))
end)
 in ( LrTable.NT 0, ( result, CONST1left, TYPELIST1right), rest671)

end
|  ( 38, ( ( _, ( MlyValue.EXPRESSION EXPRESSION2, _, EXPRESSION2right
)) :: ( _, ( MlyValue.BINOP BINOP1, _, _)) :: ( _, ( 
MlyValue.EXPRESSION EXPRESSION1, EXPRESSION1left, _)) :: rest671)) =>
 let val  result = MlyValue.EXPRESSION (fn _ => let val  EXPRESSION1 =
 EXPRESSION1 ()
 val  (BINOP as BINOP1) = BINOP1 ()
 val  EXPRESSION2 = EXPRESSION2 ()
 in (AST.BinExp(BINOP, EXPRESSION1, EXPRESSION2, ["UNDEFINED"]))
end)
 in ( LrTable.NT 0, ( result, EXPRESSION1left, EXPRESSION2right), 
rest671)
end
|  ( 39, ( ( _, ( MlyValue.EXPRESSION EXPRESSION1, _, EXPRESSION1right
)) :: ( _, ( MlyValue.UNOP UNOP1, UNOP1left, _)) :: rest671)) => let
 val  result = MlyValue.EXPRESSION (fn _ => let val  (UNOP as UNOP1) =
 UNOP1 ()
 val  (EXPRESSION as EXPRESSION1) = EXPRESSION1 ()
 in (AST.UnExp(UNOP, EXPRESSION, ["UNDEFINED"]))
end)
 in ( LrTable.NT 0, ( result, UNOP1left, EXPRESSION1right), rest671)

end
|  ( 40, ( ( _, ( MlyValue.EXPRESSION EXPRESSION3, _, EXPRESSION3right
)) :: _ :: ( _, ( MlyValue.EXPRESSION EXPRESSION2, _, _)) :: _ :: ( _,
 ( MlyValue.EXPRESSION EXPRESSION1, _, _)) :: ( _, ( _, IF1left, _))
 :: rest671)) => let val  result = MlyValue.EXPRESSION (fn _ => let
 val  EXPRESSION1 = EXPRESSION1 ()
 val  EXPRESSION2 = EXPRESSION2 ()
 val  EXPRESSION3 = EXPRESSION3 ()
 in (AST.IfExp(EXPRESSION1, EXPRESSION2, EXPRESSION3, ["UNDEFINED"]))

end)
 in ( LrTable.NT 0, ( result, IF1left, EXPRESSION3right), rest671)
end
|  ( 41, ( ( _, ( MlyValue.TYPELIST TYPELIST1, _, TYPELIST1right)) ::
 _ :: _ :: ( _, ( MlyValue.ARGUMENTLIST ARGUMENTLIST1, _, _)) :: _ :: 
( _, ( MlyValue.ID ID1, ID1left, _)) :: rest671)) => let val  result =
 MlyValue.EXPRESSION (fn _ => let val  (ID as ID1) = ID1 ()
 val  (ARGUMENTLIST as ARGUMENTLIST1) = ARGUMENTLIST1 ()
 val  (TYPELIST as TYPELIST1) = TYPELIST1 ()
 in (AST.AppExp(ID, ARGUMENTLIST, TYPELIST))
end)
 in ( LrTable.NT 0, ( result, ID1left, TYPELIST1right), rest671)
end
|  ( 42, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.ARGUMENTLIST
 ARGUMENTLIST1, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, ID1left, _)) ::
 rest671)) => let val  result = MlyValue.EXPRESSION (fn _ => let val 
 (ID as ID1) = ID1 ()
 val  (ARGUMENTLIST as ARGUMENTLIST1) = ARGUMENTLIST1 ()
 in (AST.AppExp(ID, ARGUMENTLIST, ["UNDEFINED"]))
end)
 in ( LrTable.NT 0, ( result, ID1left, RPAREN1right), rest671)
end
|  ( 43, ( ( _, ( MlyValue.TYPELIST TYPELIST1, _, TYPELIST1right)) ::
 _ :: ( _, ( MlyValue.ID ID1, ID1left, _)) :: rest671)) => let val  
result = MlyValue.EXPRESSION (fn _ => let val  (ID as ID1) = ID1 ()
 val  (TYPELIST as TYPELIST1) = TYPELIST1 ()
 in (AST.BaseExp(ID, TYPELIST))
end)
 in ( LrTable.NT 0, ( result, ID1left, TYPELIST1right), rest671)
end
|  ( 44, ( ( _, ( MlyValue.ID ID1, ID1left, ID1right)) :: rest671)) =>
 let val  result = MlyValue.EXPRESSION (fn _ => let val  (ID as ID1) =
 ID1 ()
 in (AST.BaseExp(ID, ["UNDEFINED"]))
end)
 in ( LrTable.NT 0, ( result, ID1left, ID1right), rest671)
end
|  ( 45, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.EXPRESSION 
EXPRESSION1, _, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let
 val  result = MlyValue.EXPRESSION (fn _ => let val  (EXPRESSION as 
EXPRESSION1) = EXPRESSION1 ()
 in (EXPRESSION)
end)
 in ( LrTable.NT 0, ( result, LPAREN1left, RPAREN1right), rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.PROGRAM x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : Tail_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun ID (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.ID (fn () => i),p1,p2))
fun NUM (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.NUM (fn () => i),p1,p2))
fun CONST (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.CONST (fn () => i),p1,p2))
fun ADD (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.VOID,p1,p2))
fun SUB (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun DIV (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun LESSTHAN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun GREATERTHAN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun ANDALSO (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun ORELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun XOR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun NEGATE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun NOT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun IF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
fun THEN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.VOID,p1,p2))
fun ELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(
ParserData.MlyValue.VOID,p1,p2))
fun FUN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(
ParserData.MlyValue.VOID,p1,p2))
fun DATATYPE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(
ParserData.MlyValue.VOID,p1,p2))
fun AND (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(
ParserData.MlyValue.VOID,p1,p2))
fun OF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(
ParserData.MlyValue.VOID,p1,p2))
fun RPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(
ParserData.MlyValue.VOID,p1,p2))
fun LPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(
ParserData.MlyValue.VOID,p1,p2))
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(
ParserData.MlyValue.VOID,p1,p2))
fun TIMES (p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(
ParserData.MlyValue.VOID,p1,p2))
fun ASSIGN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(
ParserData.MlyValue.VOID,p1,p2))
fun BAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(
ParserData.MlyValue.VOID,p1,p2))
fun COMMA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(
ParserData.MlyValue.VOID,p1,p2))
fun COLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 27,(
ParserData.MlyValue.VOID,p1,p2))
end
end
