(* fun iota n
	Type int -> int list
	pre: n>0
	Post: a list in a order from 0 to n
*)
fun iota 0 = []
  | iota n = (iota (n-1) @ [n-1]);