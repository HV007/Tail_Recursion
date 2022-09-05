datatype num = Zero | Successor of num

fun add (Zero: num, b: num) = b
|   add (Successor(a): num, b: num)  = Successor(add(a, b))

fun fib(Zero: num) = Zero
|   fib(Successor(Zero): num) = Successor(Zero)
|   fib(Successor(Successor(n): num): num) = add(fib(n), fib(Successor(n)))

fun ack (Zero: num, y: num) = Successor(y)
  | ack (Successor(x), Zero) = ack (x, Successor(Zero))
  | ack (Successor(x), Successor(y)) = ack (x, ack (Successor (x), y))
