(* minus n
 Type: int -> int
 PRE: n>1
 POST: First, a sum of the n first numbers/square sum of the n first numbers and then the difference between them
 EXAMPLE: sum_square_diff 10 = 2640
*)
fun sum_square_diff n =
if n < 1 then raise Domain
else
	let
	 fun sum 0 = 0
	   | sum n = n + sum (n-1)
	 fun square 0 = 0
	   | square n = n*n
	 fun square_sum 0 = 0
	   | square_sum n = square(n) + square_sum(n-1)
	in
		square(sum(n))-square_sum n
	end