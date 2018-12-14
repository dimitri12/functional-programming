(* fun6 x (y, z ,w)
 Type: int * (String * String * int) -> int * String
 PRE: x>0 and w>0
 POST: 2 outputs one of sum ofint and one of merged Strings
 EXAMPLE: fun6 1 ("Str", "ing", 2) = 3 "String"
 		  fun6 2 ("Far", "Mor", 3) = 5 "FarMor"
*)

(* VARIANT: x y z*)
fun fun6 ((x:int), (y:string, z:string, w:int)) = (x+w, y^z)
