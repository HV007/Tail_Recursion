local
datatype acc_fact = Id_fact | Cont_fact of acc_fact * (int)
fun fact_tail (n : int, k) = if (n=0) then eval_fact(k, 1) else fact_tail((n-1), Cont_fact(k, n : int))
and eval_fact (Id_fact, x) = x
| eval_fact (Cont_fact(k, n : int), w) = eval_fact (k, if (n=0) then 1 else (n*w))
in
fun fact (x_0) = fact_tail (x_0, Id_fact)
end
local
datatype acc_fib = Id_fib | Cont_fib of acc_fib * (int)
fun fib_tail (n : int, k) = if (n=0) then eval_fib(k, 0) else if (n=1) then eval_fib(k, 1) else fib_tail((n-1), Cont_fib(k, n : int))
and eval_fib (Id_fib, x) = x
| eval_fib (Cont_fib(k, n : int), w) = eval_fib (k, if (n=0) then 0 else if (n=1) then 1 else (w+fib_tail((n-2), Id_fib)))
in
fun fib (x_0) = fib_tail (x_0, Id_fib)
end

