(* fun5 x y
 Type: int -> real -> String -> String
 PRE: x>0 and y>0
 POST: ToString transformation for x and y
 EXAMPLE: fun5 2 2.0 "foo" = "foo"
 		  fun5 3 3.0 "bar" = "bar"
*)

(* VARIANT: x y z*)
fun fun5 (x:int) (y:real) (z:string) = z