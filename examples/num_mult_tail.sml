
(* use "num_add_tail.sml" *)

fun mult (Zero, b) = Zero
(*  | mult (Successor a, b) = add (mult (a, b), b) *)
  | mult (Successor a, b) = add (b, mult (a, b)) (* add b to the product of a and b *)
				
fun MULT (m, n) = toInt (mult (fromInt m, fromInt n))

local
    datatype acc_mult = Id_mult | Cont_mult of acc_mult * (num * num)

    fun mult_tail ((Zero, y), k) = eval_mult (k, Zero)
      | mult_tail ((Successor x, y), k) = mult_tail ((x, y), Cont_mult (k, (x, y)))
    and
      eval_mult (Id_mult, a) = a
    | eval_mult (Cont_mult (k, (x,y)), a) = eval_mult (k, add_TR (y,a)) (* Note (y, a) *)
in
fun mult_TR (a, b) = mult_tail ((a, b), Id_mult)
end

val MULT_TR = F2 mult_TR

(* Testing
[opening /home/self/sak/courses/HarshilVagadia/fib/num_mult_tail.sml]
val mult = fn : num * num -> num
val MULT = fn : int * int -> int
val mult_TR = fn : num * num -> num
val MULT_TR = fn : int * int -> int
val it = () : unit
- MULT_TR (0, 0);
val it = 0 : int
- MULT_TR (0, 1);
val it = 0 : int
- MULT_TR (0, 2);
val it = 0 : int
- MULT_TR (1, 0);
val it = 0 : int
- MULT_TR (2, 0);
val it = 0 : int
- MULT_TR (1, 2);
val it = 2 : int
- MULT_TR (2, 1);
val it = 2 : int
- MULT_TR (2, 3);
val it = 6 : int
- MULT_TR (3, 2);
val it = 6 : int
- 
*)    

fun mult_TR' (a, b) =
    let
	datatype acc_mult = Id_mult | Cont_mult of acc_mult * (num * num)

	fun mult_tail ((Zero, y), k) = eval_mult (k, Zero)
	  | mult_tail ((Successor x, y), k) = mult_tail ((x, y), Cont_mult (k, (x, y)))
	and
	eval_mult (Id_mult, a) = a
	| eval_mult (Cont_mult (k, (x,y)), a) = eval_mult (k, add_TR (y,a)) (* Note (y, a) *)

    in mult_tail ((a, b), Id_mult)
end

val MULT_TR' = F2 mult_TR'
