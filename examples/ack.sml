exception Undefined

fun ack (x, y) = 
    if (x < 0) orelse (y < 0) then raise Undefined
    else if x = 0 then y+1
    else if y = 0 then ack (x-1, 1)
    else ack (x-1, ack (x, y-1))

(* These are direct translations from the num datatype used in "ackermann.sml" to the int *)
	     
local
    datatype acc = Id | Cont of acc * int
    fun ack_tail (0, y, k) = eval (k, y+1)
    |   ack_tail (x, 0, k) = ack_tail (x-1, 1, k)
    |   ack_tail (x, y, k) = if (x < 0) orelse (y < 0) then raise Undefined (* Note *)
			     else ack_tail (x, y-1, Cont (k, x-1))
    and eval (Id, y) = y
    |   eval (Cont (k, x), y) = eval (k, ack_tail (x, y, Id))
    
in
fun ack_TR (x, y) = ack_tail (x, y, Id)
end

local
    datatype acc = Id | Cont of acc * int
    fun ack_tail (0, y, k) = eval (k, y+1)
    |   ack_tail (x, 0, k) = ack_tail (x-1, 1, k)
    |   ack_tail (x, y, k) = ack_tail (x, y-1, Cont (k, x-1))
    and eval (Id, y) = y
    |   eval (Cont (k, x), y) = eval (k, ack_tail (x, y, Id))
    
in
fun ack_TR2 (x, y) = if (x < 0) orelse (y < 0) then raise Undefined (* Note *)
		    else ack_tail (x, y, Id)
end

(* For testing *)
(* printing values *)
val i2s = Int.toString
	      
fun ackprint (x, y) =
    print ("ack ("^(i2s x)^", "^(i2s y)^") |-> "^(i2s (ack (x, y)))^"\n");


fun ack_TRprint (x, y) =
    print ("ack_TR ("^(i2s x)^", "^(i2s y)^") |-> "^(i2s (ack_TR (x, y)))^"\n");

fun ack_TR2print (x, y) =
    print ("ack_TR2 ("^(i2s x)^", "^(i2s y)^") |-> "^(i2s (ack_TR (x, y)))^"\n");
val L = [(0,0), (0, 1), (0, 2), (1, 0), (1,1), (1,2), (2, 0), (2,1), (2, 2), (2, 3), (2,4)];

map ackprint L;
map ack_TRprint L;
map ack_TR2print L;

(* Testing 

exception Undefined
val ack = fn : int * int -> int
val ack_TR = fn : int * int -> int
val ack_TR2 = fn : int * int -> int
val i2s = fn : int -> string
val ackprint = fn : int * int -> unit
val ack_TRprint = fn : int * int -> unit
val ack_TR2print = fn : int * int -> unit
val L = [(0,0),(0,1),(0,2),(1,0),(1,1),(1,2),(2,0),(2,1),(2,2),(2,3),(2,4)]
  : (int * int) list
ack (0, 0) |-> 1
ack (0, 1) |-> 2
ack (0, 2) |-> 3
ack (1, 0) |-> 2
ack (1, 1) |-> 3
ack (1, 2) |-> 4
ack (2, 0) |-> 3
ack (2, 1) |-> 5
ack (2, 2) |-> 7
ack (2, 3) |-> 9
ack (2, 4) |-> 11
val it = [(),(),(),(),(),(),(),(),(),(),()] : unit list
ack_TR (0, 0) |-> 1
ack_TR (0, 1) |-> 2
ack_TR (0, 2) |-> 3
ack_TR (1, 0) |-> 2
ack_TR (1, 1) |-> 3
ack_TR (1, 2) |-> 4
ack_TR (2, 0) |-> 3
ack_TR (2, 1) |-> 5
ack_TR (2, 2) |-> 7
ack_TR (2, 3) |-> 9
ack_TR (2, 4) |-> 11
val it = [(),(),(),(),(),(),(),(),(),(),()] : unit list
ack_TR2 (0, 0) |-> 1
ack_TR2 (0, 1) |-> 2
ack_TR2 (0, 2) |-> 3
ack_TR2 (1, 0) |-> 2
ack_TR2 (1, 1) |-> 3
ack_TR2 (1, 2) |-> 4
ack_TR2 (2, 0) |-> 3
ack_TR2 (2, 1) |-> 5
ack_TR2 (2, 2) |-> 7
ack_TR2 (2, 3) |-> 9
ack_TR2 (2, 4) |-> 11
val it = [(),(),(),(),(),(),(),(),(),(),()] : unit list
val it = () : unit
- 

*)
