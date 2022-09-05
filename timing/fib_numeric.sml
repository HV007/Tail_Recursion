fun fib(0) = 0
|   fib(1) = 1
|   fib(n) = fib(n-1) + fib(n-2)


local
    datatype acc_fib = Id_fib | Cont_fib of acc_fib * int
    fun fib_tail  (0, k) = eval_fib(k, 0)
    | fib_tail  (1, k) = eval_fib(k, 1)
    | fib_tail (n, k) = fib_tail(n-2, Cont_fib(k, n))
    and eval_fib (Id_fib, x) = x
    | eval_fib (Cont_fib(k, n), w) = eval_fib (k, w + fib_tail(n-1, Id_fib))
in
    fun fibTR (x_0) = fib_tail (x_0, Id_fib)
end

local
    fun fib_tail(0, a, b) = b
    |   fib_tail(n, a, b) = fib_tail(n-1, a+b, a)
in
    fun fibTR' (n) = fib_tail(n, 1, 0)
end

fun fibFromTo (f, k, l) = 
    if k > l then print ("Done\n")
    else let val fibk = f (k)
	  in  ( print (Int.toString (k)^" |-> "); 
		print (Int.toString (fibk)^"\n"); 
                             fibFromTo (f, k+1, l)
              )
         end;

fun fibUpto f n = fibFromTo (f, 0, n);