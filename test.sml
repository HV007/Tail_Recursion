datatype num = Zero | Successor of num

fun add (Zero: num, b: num) = b
|   add (Successor(a): num, b: num)  = Successor(add(a, b))

fun fib (n) = if n = Zero then Zero else if n = Successor(Zero) then Successor(zero) else if n = Successor(Successor(k)) then add(fib(Successor(k)), fib(k)) else Zero