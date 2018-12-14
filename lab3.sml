(*Assignment 3 Functional programming by Dimitri Gharam*)

(* Exercise 1*)

(* average' x y list
TYPE: real -> real -> real list -> real
PRE: none
POST: The sum of the numbers in the list plus the initial value of sum, divided by the amount of numbers in the list plus the initial value of length. 
EXAMPLE: average' 0.0 0.0 [3.333,9.999] = 6.666
         average' 2.3 5.9 [3.333,9.999] = 4.4728372093
*)
(*VARIANT: The length of the list that is the third argument.*)
fun average' length sum [] = sum / length
  | average' length sum (x::xs) = average' (length + 1.0) (sum + x) xs;

(* Exercise 2*)

(* average list
TYPE: real list -> real
PRE: none
POST: The average value of all the values in the list. 
EXAMPLE: average [1.0,4.0,10.0] = 5.0
         average [3.333, 9.999] = 6.666
*)
fun average [] = 0.0
  | average (x::xs) = average' 0.0 0.0 (x::xs);

(* Exercise 3*)

(* Exercise 3.1 append*)

(* append l1 l2
TYPE: 'a list -> 'a list -> 'a list
PRE: none
POST: A new list that includes its contents to another list.
EXAMPLE: append' [1,2,3] [4,5,6] = [1,2,3,4,5,6]
 *) 

fun append l1 [] = l1
  | append [] l2 = l2
  | append l1 l2 = foldr (fn (l2_head, new_list) => (l2_head::new_list)) l2 l1

(* Exercise 3.2 member*)

(* member x list
TYPE: ''a -> ''a list -> bool
PRE: none
POST: true if x is a member of list, false if not.
EXAMPLE: member 1 [1,2,3] = true
         member "d" ["a", "b", "c"] = false
 *)

fun member x [] = false
  | member x (head::xs) = x = (foldl (fn (y,z) => if x=y then y else z) head xs)

(* Exercise 3.3 last*)

(* last list
TYPE: 'a list -> 'a
PRE: none
POST: returns the last element of the specified list. Raises an Empty error if list is empty.
EXAMPLE: last [1,2] = 2
             last [] = Exception- Empty raised
 *)

fun last [] = raise Empty
  | last (x::xs) = foldl (fn (z,y) => z ) x xs

(* Exercise 3.4 reverse*)

(* reverse list
TYPE: 'a list -> 'a list
PRE: none
POST: a list where the order of its contents has been reversed
SIDE EFFECTS: none
EXAMPLE: reverse [1,2,3] = [3,2,1]
 *)   

fun reverse list = foldl (fn (list_head, reversed_list) => list_head::reversed_list) [] list

(* Exercise 3.5 filter*)

(* filter f list
TYPE: ('a -> bool) -> 'a list -> 'a list
PRE: none
POST: a new list that consists of the elements in the specified list for where f has returned true.
EXAMPLE: filter (fn x => x=5) [1,2,5,5,6] = [5,5]
 *)

fun filter f list = foldr (fn (l_head, new_list) => if (f l_head) then (l_head::new_list) else new_list) [] list


(* Exercise 4*)

datatype tree = Void | Node of tree * int * tree;

val ex1 = Node (Node (Node (Void , 0 ,Node (Void ,2 ,Void)),
		      3,
		      Node (Void, 5, Void)),
		6,
		Node (Void,
		      7,
		      Node (Void, 8, Node (Void, 9, Void))));

(* sub_tree a b t
TYPE: int -> int -> tree -> tree
PRE: none
POST: takes a tree and two variables a and b and returns a subtree with only the keys in the tree that are greater than or equal to a but smaller than b
SIDE EFFECTS: none
EXAMPLE: sub_tree 5 8 ex1 = Node (Node (Void ,5 ,Void), 6 ,Node (Void, 7, Void))
         sub_tree 10 20 ex1 = Void
*)
(*VARIANT: The amount of nodes unchecked in the intial tree t.*)
fun sub_tree _ _ Void = Void
  | sub_tree a b (t as Node (left, label, right)) =
	if (label >= b) then (sub_tree a b left)
	else if (label < a) then (sub_tree a b right)
	else Node (sub_tree a b left, label, sub_tree a b right);

(* Exercise 5 *)

(*If we had the function sub_tree with a and b as fixed variables, its complexity would depend solely on the height of the tree and how many subtrees it yields, that is O(n) i.e. linear time complexity. *)