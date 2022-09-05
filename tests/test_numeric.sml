fun fact (n: int) = if n = 0 then 1 else n * fact (n - 1)

fun fib (n: int) = if n = 0 then 0 else if n = 1 then 1 else fib (n - 1) + fib (n - 2)