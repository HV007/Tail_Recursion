datatype num = Zero | Successor of num

fun toInt (Zero:num) = 0 
  | toInt (Successor x) = 1+ (toInt x)

exception negativeInteger
	      
fun fromInt 0 = Zero 
  | fromInt n = if n < 0 then raise negativeInteger else Successor (fromInt (n-1))


fun add (Zero, b) = b
|   add (Successor a, b)  = Successor(add(a, b))

fun ADD (m, n) = toInt (add (fromInt m, fromInt n))

(* Testing

Standard ML of New Jersey v110.79 [built: Sat Oct 26 12:27:04 2019]
- [opening /home/self/sak/courses/HarshilVagadia/fib/num_add_tail.sml]
datatype num = Successor of num | Zero
val toInt = fn : num -> int
exception negativeInteger
val fromInt = fn : int -> num
val add = fn : num * num -> num
val ADD = fn : int * int -> int
val add_TR = fn : num * num -> num
val it = () : unit
- ADD (0, 0);
val it = 0 : int
- ADD (0, 1);
val it = 1 : int
- ADD (1, 0);
val it = 1 : int
- ADD (1, 1);
val it = 2 : int
- ADD (2, 3);
val it = 5 : int
- ADD (3, 2);
val it = 5 : int
-
*)

local
    datatype acc_add = Id_add | Cont_add of acc_add * (num * num)

    fun add_tail ((Zero, y), k) = eval_add (k, y)
      | add_tail ((Successor x, y), k) = add_tail ((x, y), Cont_add (k, (x, y)))
    and
      eval_add (Id_add, x) = x
    | eval_add (Cont_add (k, y), x) =  eval_add (k, Successor x)
in
fun add_TR (a, b) = add_tail ((a, b), Id_add)
end

fun ADD_TR (m, n) = toInt (add_TR (fromInt m, fromInt n))

(* Testing 

val ADD_TR = fn : int * int -> int
val it = () : unit
- ADD_TR (0, 0);
val it = 0 : int
- ADD_TR (0, 1);
val it = 1 : int
- ADD_TR (1, 0);
val it = 1 : int
- ADD_TR (1, 1);
val it = 2 : int
- ADD_TR (2, 3);
val it = 5 : int
- ADD_TR (3,2);
val it = 5 : int
- 

*)

(* Begin added by Harshil *)

fun mul (Zero, b) = Zero
|   mul (Successor a, b) = add(b, mul(a, b))

fun MUL (a, b) = toInt (mul (fromInt a, fromInt b))

(* Testing

- MUL (0, 0);
val it = 0 : int
- MUL (0, 1);
val it = 0 : int
- MUL (1, 0);
val it = 0 : int
- MUL (1, 1);
val it = 1 : int
- MUL (2, 3);
val it = 6 : int
- MUL (3, 2);
val it = 6 : int

*)

local
  datatype acc_mul = Id_mul | Cont_mul of acc_mul * (num * num)
  fun mul_tail ((Zero, b), k) = eval_mul (k, Zero)
  |   mul_tail ((Successor a, b), k) = mul_tail ((a, b), Cont_mul (k, (a, b)))
  and
      eval_mul (Id_mul, c) = c
  |   eval_mul (Cont_mul (k, (a, b)), c) = eval_mul (k, add_TR (b, c))
in 
  fun mul_TR (a, b) = mul_tail ((a, b), Id_mul)
end

fun MUL_TR (a, b) = toInt (mul_TR (fromInt a, fromInt b))

(* Testing

- MUL_TR (0, 1);
val it = 0 : int
- MUL_TR (0, 0);
val it = 0 : int
- MUL_TR (1, 0);
val it = 0 : int
- MUL_TR (1, 1);
val it = 1 : int
- MUL_TR (2, 3);
val it = 6 : int
- MUL_TR (3, 2);
val it = 6 : int

*)

(* End added by Harshil *)