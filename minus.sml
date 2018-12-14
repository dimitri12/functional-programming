(* minus n
 Type: int * int -> int
 PRE: x≥y && x≥0 && y≥0
 POST: A subtraction of x and y
 EXAMPLE: minus 2 2 = 0
 		  minus 3 2 = 1
 		  minus 1 0 = 0
*)
(* VARIANT: x and y *)
fun minus x y = x-y