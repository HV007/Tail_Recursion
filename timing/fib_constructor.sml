datatype num = Zero | Successor of num

(* begin added by sak *)

fun toInt (Zero:num) = 0 
  | toInt (Successor x) = 1+ (toInt x)

exception negativeInteger

fun fromInt 0 = Zero 
  | fromInt n = if n < 0 then raise negativeInteger else Successor (fromInt (n-1))

(* end added by sak *)


fun add (Zero: num, b: num) = b
|   add (Successor(a): num, b: num)  = Successor(add(a, b))

fun fib(Zero: num) = Zero
|   fib(Successor(Zero): num) = Successor(Zero)
|   fib(Successor(Successor(n): num): num) = add(fib(n), fib(Successor(n)))

val Fib = toInt o fib o fromInt

local
    datatype acc_add = Id_add | Cont_add of acc_add * (num) * (num)
    fun add_tail  (Zero : num, b : num, k) = eval_add(k, b)
    | add_tail (Successor(a) : num, b : num, k) = add_tail(a, b, Cont_add(k, Successor(a) : num, b : num))
    and eval_add (Id_add, x) = x
    | eval_add (Cont_add(k, Successor(a) : num, b : num), w) = eval_add (k, Successor(w))
in
    fun add (x_0, x_1) = add_tail (x_0, x_1, Id_add)
end

local
    datatype acc_fib = Id_fib | Cont_fib of acc_fib * (num)
    fun fib_tail  (Zero : num, k) = eval_fib(k, Zero)
    | fib_tail  (Successor(Zero) : num, k) = eval_fib(k, Successor(Zero))
    | fib_tail (Successor(Successor(n) : num) : num, k) = fib_tail(n, Cont_fib(k, Successor(Successor(n) : num) : num))
    and eval_fib (Id_fib, x) = x
    | eval_fib (Cont_fib(k, Successor(Successor(n) : num) : num), w) = eval_fib (k, add(w, fib_tail(Successor(n), Id_fib)))
in
    fun fibTR (x_0) = fib_tail (x_0, Id_fib)
end

val FibTR = toInt o fibTR o fromInt

local
    fun fib_tail(Zero, a, b) = b
    |   fib_tail(Successor(n), a, b) = fib_tail(n, add(b, a), a)
in
    fun fibTR' (n) = fib_tail(n, Successor(Zero), Zero)
end

val FibTR' = toInt o fibTR' o fromInt

fun fibFromTo (f, k, l) = 
    if k > l then print ("Done\n")
    else let val fibk = f (k)
	  in  ( print (Int.toString (k)^" |-> "); 
		print (Int.toString (fibk)^"\n"); 
                             fibFromTo (f, k+1, l)
              )
         end;

fun fibUpto f n = fibFromTo (f, 0, n);