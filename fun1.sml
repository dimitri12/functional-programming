(* fun1 x
 Type: int -> int
 PRE: x>0
 POST: An inverse of the argument
 EXAMPLE: fun1 2 = ~2
 		  fun1 3 = ~3
*)

(* VARIANT: x*)
fun fun1 x = 
	if x = 0 
	then 0
	else ~x