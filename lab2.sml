(* reverse xs ys
TYPE: 'a list -> 'a list -> 'alist
PRE: When reverse is called externally ys has to be the empty list.
POST: concatenates xs into ys in reverse order
SIDE EFFECTS: none
EXAMPLE: reverse [3,2,1,0] [] = [0,1,2,3]
		 reverses [40, 30, 20] [50, 60] = [20, 30, 40, 50 ,60] 
*)
(*VARIANT: xs *)
fun reverse [] ys = ys
  | reverse (x::xs) ys = reverse xs (x::ys);


(* count n
TYPE: int -> int list
PRE: n >= 0
POST: A list of all previous intergers equal or larger than 0 is returned in descending order
SIDE EFFECTS: none
EXAMPLE: count 4 = [3,2,1,0]
*)
(*VARIANT: n *)
fun count 0 = []
  | count n = (n-1) :: count (n-1);

(* iota n
TYPE: int -> int list
PRE: n has to be >= 0.
POST: the numbers 0 ... n-1 in ascending order in a list.
SIDE EFFECTS: None.
EXAMPLE: iota 5 = [0,1,2,3,4]
*)
fun iota 0 = []
  | iota n = reverse (count n) [];


(* member x ys
TYPE: a'' * a'' list -> bool 
PRE: none
POST: true is returned if the element is found on the list, otherwise false
SIDE EFFECTS: none
EXAMPLE: memeber 300 [100,200,300,400] = true
		 member e [a,b,c,d] = false
NOTE: This function was copied verbatim from the slides
*)
(*VARIANT: length of ys*)
fun member (x,[]) = false
  | member (x, y::ys) = x=y orelse member (x,ys);

(* inter xs ys
TYPE: a'' list -> a'' list -> a'' list
PRE: none
POST: A new list containing repeated elements between xs and ys is returned in order of appeareance
SIDE EFFECTS: none
EXAMPLE: inter [a,b,c,d,e] [c,d,e,f,g] = [c,d,f]
		 inter [56,34,87,98,23] [12,56,78,87,23] = [56,87,23]
*)
(*VARIANT: xs*)
  fun inter [] ys = []
   |inter (x::xs) ys = if (member (x, ys)) then x :: (inter xs ys) else inter xs ys;

(* inter' xs ys
TYPE: a'' list -> a'' list -> a'' list
PRE: We assume both lists are (ascending) oredered lists
POST: A new list containing repeated elements between xs and ys is returned
SIDE EFFECTS: None
EXAMPLE: inter' [a,b,c,d,e] [c,d,e,f,g] = [c,d,f]
		 inter' [56,34,87,98,23] [12,56,78,87,23] = [56,87] (NOTICE: Lists not ordered)
 *)
(*VARIANT The length of the list.*)
   fun inter' [] ys = []
	|inter' xs [] = []  
	|inter' (x::xs) (y::ys) = 
	if (x >= y) 
	then (
		if x=y 
			then x :: (inter' xs ys) 
			else (inter' (x::xs) ys)
	) 
	else inter' xs (y::ys);

(*
fun realtime f =
let
	val rt = Timer.startRealTimer ()
	val result = f ()
	val time = Timer.checkRealTimer rt
in
	(time,result)
end
val result=
let
	val s1= iota 100000
	val s2= iota 1000000
	val slowtimeandres = realtime (fn () => inter s1 s2)
	val fasttimeandres = realtime (fn () => inter' s1 s2)
in
	(slowtimeandres,fasttimeandres)
end

*)

datatype fruit = 
	Lemon of int 
	| Banana of real
	| Apple of real;

(* sumPrice xs
TYPE: fruit list -> real -> real -> real -> real
PRE: None
POST: Returns the calculated price of the shop list
SIDE EFFECTS: None
EXAMPLE: sumPrice ([Lemon 2, Apple 2.2, Banana 2.5])
*)

(* sum price xs
TYPE: fruit list -> real -> real -> real -> real
PRE: None
POST: Returns the calculated price of the shop list
SIDE EFFECTS: None
EXAMPLE: sumPrice ([Lemon 2, Apple 2.2, Banana 2.5])
*)
fun sumPrice [] _ _ _ = 0.0
  | sumPrice ((Lemon head)::fList) a b l = (real head)*l + (sumPrice fList a b l)
  | sumPrice ((Apple head)::fList) a b l = head*a + (sumPrice fList a b l)
  | sumPrice ((Banana head)::fList) a b l = head*b + (sumPrice fList a b l);

  datatype 'a tree = Node of 'a * 'a tree list

val treeA = Node (1,[]);
val treeB = Node (1,[Node (2,[]), Node (3,[])]);
val treeC = Node (1,[Node (2,[]), Node (3,[]), Node (4,[])]);
val treeD = Node (1,[Node (2,[]), Node (3,[Node (5,[]), Node (6,[])])]);

(* sum xs
TYPE: int list -> int
PRE: None
POST: Gets a list and returns the sum of all its elements
SIDE EFFECTS: None
EXAMPLE: sum [1,2,3,4,5] = 15
         sum [6,6,6] = 18 
*)
(*VARIANT The length of the list supplied*)
fun sum [] = 0
  | sum (x::xs) = x + sum xs

(* count tree
TYPE: 'a tree -> int
PRE: None
POST: Takes a tree and returns the total number of nodes
SIDE EFFECTS: None
EXAMPLE: count (Node (1,[Node (2,[]), Node (3,[Node (5,[]), Node (6,[])])])) = 5
         count (Node (1,[Node (2,[]), Node (3,[])])) = 3

*)
(*VARIANT The amount of children that has children of their own.*)
fun count (Node (_,[])) = 1
  | count (Node (_,(children))) = 1 + sum (map count children)

(* deLister xs
TYPE: a' list list -> a' list
PRE: None
POST: Takes a list of lists, returns a single list containing all elements concatenated together
SIDE EFFECTS: None
EXAMPLE: deLister [[1],[2],[3],[4],[5]] = [1,2,3,4,5]
         deLister [[1,2,3,4],[5,6,7,8]] = [1,2,3,4,5,6,7,8]
*)
(*VARIANT The length of the list.*)					  
fun deLister [] = []
  | deLister (x::xs) = x @ (deLister xs)

(* labels tree
TYPE: 'a tree -> 'a list
PRE: None
POST: Takes a tree and returns a list containing all of the labels on all of its nodes
SIDE EFFECTS: None
EXAMPLE: labels (Node (1,[Node (2,[]), Node (3,[Node (5,[]), Node (6,[])])])) = [1,2,3,5,6]
         labels (Node (1,[Node (2,[]), Node (3,[])])) = [1,2,3]
*)
(*VARIANT The amount of children that has children of their own.*)			       
fun labels (Node (x,[])) = [x]
  | labels (Node (x,children)) = x::(deLister (map labels children))

(* has_true list
TYPE: bool list -> bool
PRE: None
POST: Takes a list of booleans, returns true if it contains a true as at least one of its elements
SIDE EFFECTS: None
EXAMPLE: has_true [true,false,true,true] = true
         has_true [false, false, false] = false
*)
(*VARIANT The length of the list.*)					
fun has_true [] = false
  | has_true (x::xs) = x orelse has_true xs

(* is_present tree
TYPE: ''a tree -> ''a -> bool
PRE: None
POST: Takes a tree and a single element, returns true if the element is present on the tree, otherwise it returns false
SIDE EFFECTS: None
EXAMPLE: is_present 4 (Node (1,[Node (2,[]), Node (3,[Node (5,[]), Node (6,[])])])) = false
         is_present 5 (Node (1,[Node (2,[]), Node (3,[Node (5,[]), Node (6,[])])])) = true
*)
(*VARIANT The amount of children that has children of their own.*)					
fun is_present (Node (label,[])) x = if (label=x) then true else false
  | is_present (Node (label, children)) x =
    if (label=x)
    then true
    else has_true (map (fn y => is_present y x) children)

(* largest xs
TYPE: int list -> int
PRE: None
POST: Takes a list of integers and returns the largest element on the list
SIDE EFFECTS: None
EXAMPLE: largest [100,200,300,999] = 999
         largest [341, 54253, 883] = 54253
*)
(*VARIANT The length of the list.*)		  
fun largest (x::[]) = x
  | largest (x::xs) = if x>(largest xs) then x else largest xs

(* height xs
TYPE: a' tree -> int
PRE: None
POST: Takes a tree and returns the height
SIDE EFFECTS: None
EXAMPLE: height (Node (1,[Node (2,[]), Node (3,[Node (5,[]), Node (6,[])])])) = 3
         height (Node (1,[])) = 1

*)
(*VARIANT The amount of children that has children of their own.*)							    
fun height (Node (_,[])) = 1
  | height (Node (_,children)) = 1 + largest (map height children)
