datatype num = Zero | Successor of num

(* begin added by sak *)

fun toInt (Zero:num) = 0 
  | toInt (Successor x) = 1+ (toInt x)

exception negativeInteger

fun fromInt 0 = Zero 
  | fromInt n = if n < 0 then raise negativeInteger else Successor (fromInt (n-1))

(* end added by sak *)
				    
datatype acc = Id | Cont of acc * num

fun add(a: num, b: num): num =
    case a of
        Zero => b
    |   Successor c => Successor(add(c,b))

fun fib_tail(n: num, k: acc): num = 
    case n of
        Zero => eval(k, Zero)
    |   Successor Zero => eval(k, Successor Zero)
    |   Successor(Successor a) => fib_tail(Successor a, Cont(k, a))
and
eval(k: acc, n: num) = 
    case k of
        Id => n
    |   Cont(k, a) => eval(k, add(n, fib_tail(a, Id)))

fun fib(n: num): num = fib_tail(n, Id)

(* begin added by sak *)
val Fib = toInt o fib o fromInt

fun fibFromTo (f, k, l) = 
    if k > l then print ("Done\n")
    else let val fibk = f (k)
	  in  ( print (Int.toString (k)^" |-> "); 
		print (Int.toString (fibk)^"\n"); 
                             fibFromTo (f, k+1, l)
              )
         end;

fun fibUpto f n = fibFromTo (f, 0, n);
(* end added by sak *)
