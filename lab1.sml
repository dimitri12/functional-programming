(*Functional programming assignment 1 - By Dimitri Gharam*)

(*NOTE!!: i have been using SML instead of POLY/ML by using a brew install on Mac (i am a Mac user)*)

(* "1a" Evaluation of functionality of the product function:

>val foo =product 3;
->if 3=1? = false
-> 3 * product(2)
->if 2=1? = false
->2*product(1)
->if 1=1? =true
->foo = 3*2*1 = 6
*)

(*"1b" The Product function computes the faculity of n i.e. n! in the mathematical language*)

(* (1c) specification of product n
 Type: int->int
 PRE: n≥0
 POST: n!
 EXAMPLE: product 0 = 1
 		  product 3 = 6
*)

fun product n = 
	if n = 1 
	then 1 
	else 
		n*product(n-1)

(* "1D" Variant of the product function:
	fun product 0 = 1
	  | product n = n*product(n-1)
 *)

(*Task 2: Currying*)

(*Task 2.1, the reason to perform this task is the ability to pass functions as variables and perform currying which passes one argument to a function and returning a function stored in a variable so whenever that variable is called you can pass a new argument with that variable to execute the function*)

(* minus n
 Type: int * int -> int
 PRE: x≥y && x≥0 && y≥0
 POST: A subtraction of x and y
 EXAMPLE: minus 2 2 = 0
 		  minus 3 2 = 1
 		  minus 1 0 = 0
*)

val minus =fn x => fn y => x-y

(*Task 2.2: when "minus 5 4" is entered, the function minus is called and the parameters 5 and 4 
are inserted as x=5 and y=4 and the result will generate a subtraction of these variables that will be stored in foo=1*)

(*Task 2.3: The function will return an error since we didn't send enough data to the function to start executing the function*)

(*Task 2.4: 
>val foo = minus 5 4;
->x=5 and y=4
->foo = 5-4 =1

*)


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

(* fun2 x y
 Type: int -> int -> int
 PRE: x≥0 and y≥0
 POST: An positive increment of x and y
 EXAMPLE: fun2 2 2 = 4
 		  fun2 3 3 = 6
*)

fun fun2 x y = x+y

(* fun3 x
 Type: int -> int * int
 PRE: true
 POST: An different writing of x twice
 EXAMPLE: fun3 2 = (2, 2)
 		  fun3 3 = (3, 3)
*)

(* VARIANT: x*)
fun fun3 (x : int) = (x, x)

(* fun4 x y
 Type: int * int -> int
 PRE: x≥0, y≥0
 POST: An positive sum of x and y
 EXAMPLE: fun3 (2, 2) = 4
 		  fun3 (3, 3) = 6
*)

(* VARIANT: x*)
fun fun4 (x, y) = x+y

(* fun5 x y
 Type: int -> real -> String -> String
 PRE: x>0 and y>0
 POST: find the string in the inputs
 EXAMPLE: fun5 2 2.0 "foo" = "foo"
 		  fun5 3 3.0 "bar" = "bar"
*)

(* VARIANT: x y z*)
fun fun5 (x:int) (y:real) (z:string) = z

(* fun6 x (y, z ,w)
 Type: int * (String * String * int) -> int * String
 PRE: x>0 and w>0
 POST: 2 outputs one of sum of int and one of merged Strings
 EXAMPLE: fun6 1 ("Str", "ing", 2) = 3 "String"
 		  fun6 2 ("Far", "Mor", 3) = 5 "FarMor"
*)

(* VARIANT: x y z*)
fun fun6 ((x:int), (y:string, z:string, w:int)) = (x+w, y^z)

(* minus n
 Type: int -> int
 PRE: n>1
 POST: The difference between a squared sum and a sum in square of a series of numbers
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