datatype num = Zero | Successor of num

fun toInt (Zero:num) = 0 
  | toInt (Successor x) = 1+ (toInt x)

exception negativeInteger
	      
fun fromInt 0 = Zero 
  | fromInt n = if n < 0 then raise negativeInteger else Successor (fromInt (n-1))

fun ack (Zero, y) = Successor y
  | ack (Successor x, Zero) = ack (x, Successor Zero)
  | ack (Successor x, Successor y) = ack (x, ack (Successor x, y))

fun ACK (x, y) = toInt (ack (fromInt x, fromInt y))

(* Testing:

- ACK (0, 0);
val it = 1 : int
- ACK (0, 1);
val it = 2 : int
- ACK (1, 0);
val it = 2 : int
- ACK (1, 1);
val it = 3 : int
- ACK (2, 3);
val it = 9 : int
- ACK (3, 2);
val it = 29 : int

*)

local
    datatype acc = Id | Cont of acc * num
    fun ack_tail (Zero, y, k) = eval (k, Successor y)
    |   ack_tail (Successor x, Zero, k) = ack_tail (x, Successor Zero, k)
    |   ack_tail (Successor x, Successor y, k) = ack_tail (Successor x, y, Cont (k, x))
    and eval (Id, y) = y
    |   eval (Cont (k, x), y) = eval (k, ack_tail (x, y, Id))
in
    fun ack_TR (x, y) = ack_tail (x, y, Id)
end

fun ACK_TR (x, y) = toInt (ack_TR (fromInt x, fromInt y))

(* Testing:

- ACK_TR (0, 0);
val it = 1 : int
- ACK_TR (0, 1);
val it = 2 : int
- ACK_TR (1, 0);
val it = 2 : int
- ACK_TR (1, 1);
val it = 3 : int
- ACK_TR (2, 3);
val it = 9 : int
- ACK_TR (3, 2);

*)