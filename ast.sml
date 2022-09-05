structure AST =
struct

exception EXP

datatype FunctionType = NonRecursive | Recursive | TailRecursive
datatype binop = Add | Sub | Mul | Div | Equals | LessThan | GreaterThan | And | Or | Xor
and unop = Negate | Not

(* datatype ConstructorExp = BaseExp of string * (string list) | AppExp of string * (ConstructorExp list) * (string list) *)
(* and ArgumentList = ConstructorExp list *)
datatype Rewrite = RewriteRule of string * (exp list) * exp
(* and Function = Rewrite list *)
(* and Type = string list *)
and ConstructorDef = BaseDef of string | AppDef of string * (string list)
and Datatype = DatatypeDef of string * (ConstructorDef list)
and Program = ProgramDef of (Datatype list) * (((Rewrite list) list) list)
and exp = NumExp of int * (string list)
		| BoolExp of bool * (string list)
		| BinExp of binop * exp * exp * (string list)
		| UnExp of unop * exp * (string list)
		| IfExp of exp * exp * exp * (string list)
        | BaseExp of string * (string list)
        | AppExp of string * (exp list) * (string list)

val flag = ref 0

fun binopToString(binop) = case binop of
    Add => "+"
|   Sub => "-"
|   Mul => "*"
|   Div => "/"
|   Equals => "="
|   LessThan => "<"
|   GreaterThan => ">"
|   And => "andalso"
|   Or => "orelse"
|   Xor => "^"
and unopToString(unop) = case unop of
    Negate => "~"
|   Not => "not"

fun classifyProgram(ProgramDef(dataTypeList, functionDeclarationList)) = classifyFunctionDeclarationList(functionDeclarationList)
and classifyFunctionDeclarationList([]) = []
|   classifyFunctionDeclarationList(functionDeclaration :: functionDeclarationList) = classifyFunctionList(functionDeclaration) :: classifyFunctionDeclarationList(functionDeclarationList)
and classifyFunctionList([]) = []
|   classifyFunctionList(function :: functionList) =  classifyFunction(function) :: classifyFunctionList(functionList)
and classifyFunction([]) = NonRecursive
|   classifyFunction(rewrite :: rewriteList) = case (classifyRewrite(rewrite), classifyFunction(rewriteList)) of
    (Recursive, _) => Recursive
|   (_, Recursive) => Recursive
|   (TailRecursive, _) => TailRecursive
|   (_, TailRecursive) => TailRecursive
|   (NonRecursive, NonRecursive) => NonRecursive
and classifyRewrite(RewriteRule(funcName, arguments, exp)) = case exp of
    NumExp(varName, _) => NonRecursive
|   BoolExp(varName, _) => NonRecursive
|   BinExp(varName, exp1, exp2, _) => if searchInExp(exp1, funcName) orelse searchInExp(exp2, funcName) then Recursive else NonRecursive
|   UnExp(varName, exp, _) => if searchInExp(exp, funcName) then Recursive else NonRecursive
|   IfExp(exp1, exp2, exp3, _) => if searchInExp(exp1, funcName) orelse searchInExp(exp2, funcName) orelse searchInExp(exp3, funcName) then Recursive else NonRecursive
|   BaseExp(str, _) => if str = funcName then TailRecursive else NonRecursive
|   AppExp(cons, arguments, _) => if searchInArguments(arguments, funcName) then Recursive else if cons = funcName then TailRecursive else NonRecursive
and searchInExp(exp, funcName) = case exp of
    NumExp(varName, _) => false
|   BoolExp(varName, _) => false
|   BinExp(varName, exp1, exp2, _) => searchInExp(exp1, funcName) orelse searchInExp(exp2, funcName)
|   UnExp(varName, exp, _) => searchInExp(exp, funcName)
|   IfExp(exp1, exp2, exp3, _) => searchInExp(exp1, funcName) orelse searchInExp(exp2, funcName) orelse searchInExp(exp3, funcName)
|   BaseExp(str, _) => str = funcName
|   AppExp(cons, arguments, _) => cons = funcName orelse searchInArguments(arguments, funcName)
and searchInArguments([], funcName) = false
|   searchInArguments(argument :: arguments, funcName) = searchInExp(argument, funcName) orelse searchInArguments(arguments, funcName)

fun outputDatatypeList([]) = ""
|   outputDatatypeList([dataType]) = outputDatatype(dataType)
|   outputDatatypeList(dataType :: dataTypeList) = (outputDatatype(dataType) ^ "and " ^ outputDatatypeList(dataTypeList))
and outputDatatype(DatatypeDef(name, construcorDefList)) = ("datatype " ^ name ^ " = " ^ outputConstructorDefList(construcorDefList) ^ "\n")
and outputConstructorDefList([]) = raise EXP
|   outputConstructorDefList([constructorDef]) = outputConstructorDef(constructorDef)
|   outputConstructorDefList(constructorDef ::  constructorDefList) = (outputConstructorDef(constructorDef) ^ " | " ^ outputConstructorDefList(constructorDefList))
and outputConstructorDef(BaseDef str) = str
|   outputConstructorDef(AppDef(str, typeList)) = (str ^ " of " ^ outputTypeList(typeList))
and outputTypeList([]) = raise EXP
|   outputTypeList([Type]) = Type
|   outputTypeList(Type :: typeList) = (Type ^ " * " ^ outputTypeList(typeList))

fun outputFunctionDeclaration([]) = raise EXP
|   outputFunctionDeclaration([function]) = outputFunction(function)
|   outputFunctionDeclaration(function :: functionDeclaration) = (outputFunction(function) ^ "and " ^ outputFunctionDeclaration(functionDeclaration))
and outputFunction([]) = raise EXP
|   outputFunction([rewrite]) = outputRewrite(rewrite) ^ "\n"
|   outputFunction(rewrite :: rewriteList) = (outputRewrite(rewrite) ^ "\n | " ^ outputFunction(rewriteList))
and outputRewrite(RewriteRule(name, expList, exp)) = (name ^ " (" ^ outputExpList(expList) ^ ") = " ^ outputExp(exp))
and outputExpList([]) = raise EXP
|   outputExpList([exp]) = outputExp(exp)
|   outputExpList(exp :: expList) = (outputExp(exp) ^ ", " ^ outputExpList(expList))
and outputExp(exp) = case exp of
    NumExp(num, typeList) => if typeList = ["UNDEFINED"] then Int.toString(num) else Int.toString(num) ^ " : " ^ outputTypeList(typeList)
|   BoolExp(bool, typeList) => if typeList = ["UNDEFINED"] then Bool.toString(bool) else Bool.toString(bool) ^ " : " ^ outputTypeList(typeList)
|   BinExp(binop, exp1, exp2, typeList) => if typeList = ["UNDEFINED"] then ("(" ^ outputExp(exp1) ^ binopToString(binop) ^ outputExp(exp2) ^ ")") else ("(" ^ outputExp(exp1) ^ binopToString(binop) ^ outputExp(exp2) ^ ")" ^ " : " ^ outputTypeList(typeList))
|   UnExp(unop, exp, typeList) => if typeList = ["UNDEFINED"] then (unopToString(unop) ^ outputExp(exp)) else ("(" ^ unopToString(unop) ^ outputExp(exp) ^ ")" ^ " : " ^ outputTypeList(typeList))
|   IfExp(exp1, exp2, exp3, typeList) => if typeList = ["UNDEFINED"] then ("if " ^ outputExp(exp1) ^ " then " ^ outputExp(exp2) ^ " else " ^ outputExp(exp3)) else ("(" ^ "if " ^ outputExp(exp1) ^ " then " ^ outputExp(exp2) ^ " else " ^ outputExp(exp3) ^ ")" ^ " : " ^ outputTypeList(typeList))
|   BaseExp(str, typeList) => if typeList = ["UNDEFINED"] then str else str ^ " : " ^ outputTypeList(typeList)
|   AppExp(cons, arguments, typeList) => if typeList = ["UNDEFINED"] then (cons ^ "(" ^ outputExpList(arguments) ^ ")") else (cons ^ "(" ^ outputExpList(arguments) ^ ")" ^ " : " ^ outputTypeList(typeList))

fun iterateArguments([], num) = ""
|   iterateArguments([argument], num) = "x_" ^ Int.toString(num)
|   iterateArguments(argument :: argumentList, num) = ("x_" ^ Int.toString(num) ^ ", " ^ iterateArguments(argumentList, num + 1))

fun outputExpType(exp) = case exp of
    NumExp(num, typeList) => "(" ^ outputTypeList(typeList) ^ ")"
|   BoolExp(bool, typeList) => "(" ^ outputTypeList(typeList) ^ ")"
|   BinExp(binop, exp1, exp2, typeList) => "(" ^ outputTypeList(typeList) ^ ")"
|   UnExp(unop, exp, typeList) => "(" ^ outputTypeList(typeList) ^ ")"
|   IfExp(exp1, exp2, exp3, typeList) => "(" ^ outputTypeList(typeList) ^ ")"
|   BaseExp(str, typeList) => "(" ^ outputTypeList(typeList) ^ ")"
|   AppExp(cons, arguments, typeList) => "(" ^ outputTypeList(typeList) ^ ")"
and iterateDatatypes([]) = ""
|   iterateDatatypes([argument]) = outputExpType(argument) ^ "\n"
|   iterateDatatypes(argument :: argumentList) = outputExpType(argument) ^ " * " ^ iterateDatatypes(argumentList)

fun outputMainFunction([]) = raise EXP
|   outputMainFunction(RewriteRule(name, arguments, exp) :: _) = ("fun " ^ name ^ " (" ^ iterateArguments(arguments, 0) ^ ") = " ^ name ^ "_tail (" ^ iterateArguments(arguments, 0) ^ ", Id_" ^ name ^ ")\n")

fun outputAccDatatype([]) = raise EXP
|   outputAccDatatype(RewriteRule(name, arguments, exp) :: _) = ("datatype acc_" ^ name ^ " = Id_" ^ name ^ " | Cont_" ^ name ^ " of acc_" ^ name ^ " * " ^ iterateDatatypes(arguments))

fun outputEvalFunction(function) =
    let
        val func = List.nth(function, 0)
        val RewriteRule(name, arguments, exp) = func
    in
        "and " ^ "eval_" ^ name ^ " (Id_" ^ name ^ ", x) = x\n" ^ outputIndividualEvals(function)
    end
and outputIndividualEvals([]) = ""
|   outputIndividualEvals(RewriteRule(name, expList, exp) :: rewriteList) = (if classifyRewrite(RewriteRule(name, expList, exp)) = NonRecursive then ("") else 
    (flag := 0; "| eval_" ^ name ^ " (Cont_" ^ name ^ "(k" ^ outputArguments(expList) ^ ", w) = " ^ "eval_" ^ name ^ " (k, " ^ outputExp(getContext(name, exp))) ^ ")\n") ^ outputIndividualEvals(rewriteList)
and outputArguments([]) = ")"
|   outputArguments(exp :: exps) = (", " ^ outputExp(exp) ^ outputArguments(exps))
and getContext(name, exp) = case exp of
    NumExp(num, typeList) => exp
|   BoolExp(bool, typeList) => exp
|   BinExp(binop, exp1, exp2, typeList) => BinExp(binop, getContext(name, exp1), getContext(name, exp2), typeList)
|   UnExp(unop, exp, typeList) => UnExp(unop, getContext(name, exp), typeList)
|   IfExp(exp1, exp2, exp3, typeList) => IfExp(exp1, getContext(name, exp2), getContext(name, exp3), typeList)
|   BaseExp(str, typeList) => BaseExp(str, typeList)
|   AppExp(str, expList, typeList) => (let val temp = getContextArguments(name, expList) in
    if !flag = 1 then (if str = name then AppExp(name ^ "_tail", temp @ [BaseExp("Id_" ^ name, ["UNDEFINED"])], typeList) else AppExp(str, temp, typeList)) else (if str = name then (flag := 1; BaseExp("w", ["UNDEFINED"])) else AppExp(str, temp, typeList))
    end)
and getContextArguments(name, []) = []
|   getContextArguments(name, [argument]) = [getContext(name, argument)]
|   getContextArguments(name, argument :: argumentList) = getContext(name, argument) :: getContextArguments(name, argumentList)

fun outputTailFunction(function) =  ("fun " ^ outputTailRewriteList(function))
and outputTailRewriteList([]) = raise EXP
|   outputTailRewriteList([rewrite]) = (outputRewrite(convertRewrite(rewrite)) ^ "\n")
|   outputTailRewriteList(rewrite :: rewriteList) = (outputRewrite(convertRewrite(rewrite)) ^ "\n| " ^ outputTailRewriteList(rewriteList))
and convertRewrite(RewriteRule(name, expList, exp)) = if classifyRewrite(RewriteRule(name, expList, exp)) = NonRecursive then (RewriteRule(name ^ "_tail ", expList @ [BaseExp("k", ["UNDEFINED"])], AppExp("eval_" ^ name, [BaseExp("k", ["UNDEFINED"]), exp], ["UNDEFINED"]))) else (
    flag := 0; RewriteRule(name ^ "_tail", expList @ [BaseExp("k", ["UNDEFINED"])], getInnermostCall(name, expList, exp)))
and getInnermostCall(name, expList, exp) = case exp of
    NumExp(num, typeList) => (BaseExp ("NULL", ["UNDEFINED"]))
|   BoolExp(bool, typeList) => (BaseExp ("NULL", ["UNDEFINED"]))
|   BinExp(binop, exp1, exp2, typeList) => if getInnermostCall(name, expList, exp1) = (BaseExp ("NULL", ["UNDEFINED"])) then (getInnermostCall(name, expList, exp2)) else (getInnermostCall(name, expList, exp1))
|   UnExp(unop, exp, typeList) => getInnermostCall(name, expList, exp)
|   IfExp(exp1, exp2, exp3, typeList) => (let
        val rewrite1 = convertRewrite(RewriteRule(name, expList, exp2))
        val rewrite2 = convertRewrite(RewriteRule(name, expList, exp3))
        val RewriteRule(_, _, expr1) = rewrite1
        val RewriteRule(_, _, expr2) = rewrite2
    in
        IfExp(exp1, expr1, expr2, typeList)
    end)
|   BaseExp(str, typeList) => (BaseExp ("NULL", ["UNDEFINED"]))
|   AppExp(str, arguments, typeList) => (if str = name then (if getInnermostCallArgumentsList(name, expList, arguments) = BaseExp ("NULL", ["UNDEFINED"]) then AppExp(name ^ "_tail", arguments @ [AppExp("Cont_" ^ name, [BaseExp("k", ["UNDEFINED"])] @ expList, ["UNDEFINED"])], ["UNDEFINED"]) else getInnermostCallArgumentsList(name, expList, arguments)) else getInnermostCallArgumentsList(name, expList, arguments))
and getInnermostCallArgumentsList(name, expList, []) = (BaseExp ("NULL", ["UNDEFINED"]))
|   getInnermostCallArgumentsList(name, expList, argument :: argumentList) = if getInnermostCall(name, expList, argument) = (BaseExp ("NULL", ["UNDEFINED"])) then getInnermostCallArgumentsList(name, expList, argumentList) else getInnermostCall(name, expList, argument)

fun transformFunctionDeclarationList([]) = "\n"
|   transformFunctionDeclarationList(functionDeclaration :: functionDeclarationList) = (transformFunctionDeclaration(functionDeclaration) ^ transformFunctionDeclarationList(functionDeclarationList))
and transformFunctionDeclaration([function]) = if classifyFunction(function) = Recursive then transformFunction(function) else "fun " ^ outputFunctionDeclaration([function])
|   transformFunctionDeclaration(functionList) = "fun " ^ outputFunctionDeclaration(functionList)
and transformFunction(function) = ("local\n" ^ outputAccDatatype(function) ^ outputTailFunction(function) ^ outputEvalFunction(function) ^ "in\n" ^ outputMainFunction(function) ^ "end\n")

fun transformProgram(ProgramDef(datatypeList, functionDeclarationList)) = (outputDatatypeList(datatypeList) ^ transformFunctionDeclarationList(functionDeclarationList))
end
