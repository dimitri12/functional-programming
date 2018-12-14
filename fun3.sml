(* fun3 x
 Type: int -> int * int
 PRE: x≥0
 POST: An different writing of x twice
 EXAMPLE: fun3 2 = (2, 2)
 		  fun3 3 = (3, 3)
*)

(* VARIANT: x*)
fun fun3 (x : int) = (x, x)