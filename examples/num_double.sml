datatype num = Zero | Successor of num

fun toInt (Zero:num) = 0 
  | toInt (Successor x) = 1+ (toInt x)

exception negativeInteger
	      
fun fromInt 0 = Zero 
  | fromInt n = if n < 0 then raise negativeInteger else Successor (fromInt (n-1))

local
    fun f (Zero, y) = y
    |   f (Successor x, y) = Successor (Successor (g (x, y, Zero)))
    and g (Zero, y, z) = y
    |   g (Successor x, y, z) = Successor (f (x, Successor y))
in
    fun double (n) = f (n, Zero)
end

fun DOUBLE (n) = toInt (double (fromInt n))

(* Testing:

- DOUBLE 0;
val it = 0 : int
- DOUBLE 1;
val it = 2 : int
- DOUBLE 2;
val it = 4 : int
- DOUBLE 3;
val it = 6 : int
- DOUBLE 4;
val it = 8 : int
- DOUBLE 5;
val it = 10 : int
- DOUBLE 10;
val it = 20 : int

*)

local
    datatype acc = Id | Cont_f of acc | Cont_g of acc
    fun f_tail (Zero, y, k) = eval (k ,y)
    |   f_tail (Successor x, y, k) = g_tail (x, y, Zero, Cont_f k)
    and g_tail (Zero, y, z, k) = eval (k, y)
    |   g_tail (Successor x, y, z, k) = f_tail (x, Successor y, Cont_g k)
    and eval (Id, y) = y
    |   eval (Cont_f k, y) = eval (k, Successor (Successor y))
    |   eval (Cont_g k, y) = eval (k, Successor y)
in
    fun double_TR (n) = f_tail (n, Zero, Id)
end

fun DOUBLE_TR (n) = toInt (double_TR (fromInt n))

(* Testing:

- DOUBLE_TR 0;
val it = 0 : int
- DOUBLE_TR 1;
val it = 2 : int
- DOUBLE_TR 2;
val it = 4 : int
- DOUBLE_TR 3;
val it = 6 : int
- DOUBLE_TR 4;
val it = 8 : int
- DOUBLE_TR 5;
val it = 10 : int
- DOUBLE_TR 10;
val it = 20 : int

*)