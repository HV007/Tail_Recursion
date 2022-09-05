exception negativeArgument;

fun fib (n) = if n < 0 then raise negativeArgument
	      else if (n <= 1) then n
	      else fib (n-1) + fib (n-2);

local
    fun fib_iter (n, a, b, m) = (* assume fib (m) = b and fib (m-1) = a *)
	if m >= n then b
	else fib_iter (n, b, a+b, m+1);
in
fun fibTR (n) = if n < 0 then raise negativeArgument
                else if (n <= 1) then n
		else fib_iter (n, 0, 1, 1);
end;

local
    fun g (n, a, b) = if n=0 then a
		      else if n=1 then b
		      else g(n-1, b, a+b)
in
fun fibTR' n = if n < 0 then raise negativeArgument
	       else g(n, 0, 1)
end

(* Begin added by Harshil *)

local
	datatype acc = Id | Cont of acc * int
	fun fib_tail(n: int, k: acc): int = 
		if n <= 1 then eval(k, n)
		else fib_tail(n-1, Cont(k, n-2))
	and
	eval(k: acc, n: int) = 
		case k of
			Id => n
		|   Cont(k, a) => eval(k, n + fib_tail(a, Id))
in
fun fibTR'' (n: int): int = fib_tail(n, Id)
end

(* End added by Harshil *)
    
fun fibFromTo (f, k, l) = 
    if k > l then print ("Done\n")
    else let val fibk = f (k)
	  in  ( print (Int.toString (k)^" |-> "); 
		print (Int.toString (fibk)^"\n"); 
                             fibFromTo (f, k+1, l)
              )
         end;

fun fibUpto f n = fibFromTo (f, 0, n);
