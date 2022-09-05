exception negativeArgument;

fun fib (n) = if n < 0 then raise negativeArgument
              else if (n <= 1) then n
              else fib (n-1) + fib (n-2);

local
    datatype acc_fib = Id_fib | Cont_fib of acc_fib * int
					  
    fun fib_tail (n, k): int =
	if n <= 1 then eval_fib (k, n)
	else fib_tail (n-1, Cont_fib (k, n-2))
    and
      eval_fib (Id_fib, n) = n
    | eval_fib (Cont_fib (k, a), n)  = n + fib_tail (a, k)
    
in
   fun fib_TR n = fib_tail (n, Id_fib)
end

(* testing
[opening /home/self/sak/courses/HarshilVagadia/fib/fibonacci_tail.sml]
exception negativeArgument
val fib = fn : int -> int
val fib_TR = fn : int -> int
val it = () : unit
- use "testFib.sml";
[opening testFib.sml]
[autoloading]
[library $SMLNJ-BASIS/basis.cm is stable]
[library $SMLNJ-BASIS/(basis.cm):basis-common.cm is stable]
[autoloading done]
val fibFromTo = fn : (int -> int) * int * int -> unit
val fibUpto = fn : (int -> int) -> int -> unit
val it = () : unit
- fibUpto fib_TR 10;
0 |-> 0
1 |-> 1
2 |-> 1
3 |-> 2
4 |-> 3
5 |-> 5
6 |-> 8
7 |-> 13
8 |-> 21
9 |-> 34
10 |-> 55
Done
val it = () : unit
- 

*)
    
fun fib_TR' n =
    let
	datatype acc_fib = Id_fib | Cont_fib of acc_fib * int
	fun fib_tail (n, k): int =
	    if n <= 1 then eval_fib (k, n)
	    else fib_tail (n-1, Cont_fib (k, n-2))
	and
	eval_fib (Id_fib, n) = n
	| eval_fib (Cont_fib (k, a), n)  = n + fib_tail (a, k)

							      
    in fib_tail (n, Id_fib)
    end

(* testing 
[opening /home/self/sak/courses/HarshilVagadia/fib/fibonacci_tail.sml]
exception negativeArgument
val fib = fn : int -> int
val fib_TR = fn : int -> int
val fib_TR' = fn : int -> int
val it = () : unit
- use "testFib.sml";
[opening testFib.sml]
val fibFromTo = fn : (int -> int) * int * int -> unit
val fibUpto = fn : (int -> int) -> int -> unit
val it = () : unit
- fibUpto fib_TR' 10;
0 |-> 0
1 |-> 1
2 |-> 1
3 |-> 2
4 |-> 3
5 |-> 5
6 |-> 8
7 |-> 13
8 |-> 21
9 |-> 34
10 |-> 55
Done
val it = () : unit
-

*)
