(*Exercise 1*)
(*1*)
signature VALUATION =
sig
    type T
    val empty: T
    val set: T -> string -> bool -> T
    val value_of: T -> string -> bool
    val variables: T -> string list
    val print: T -> unit
end

(*Exercise 2*)
(*NOTE: My answer was inspired on the course notes "SML's Module System", slide 45, about ADT for Dictionaries*)
structure Valuation :> VALUATION =
struct
    type T = (string * bool) list
    val empty = []

    (* set
    TYPE:(''a * 'b) list -> ''a -> 'b -> (''a * 'b) list
    PRE: --
    POST: adds (string * bool) to the dictionary, if the string (key) already exist, replace boolean (value)
    SIDE EFFECTS: --
    EXAMPLE: set [] "All monkeys fly" false = [("All monkeys fly", false)]
             set [("All monkeys fly", false)] "All dogs bark" true = [("All monkeys fly", false), ("All dogs bark",true)]
             set [("All monkeys fly", false),("All dogs bark",true)] "All monkeys fly" true = [("All monkeys fly", true), ("All dogs bark",true)]
    *)
    (*VARIANT: key, value*)
    fun set [] s b  = (s,b) :: []
      | set ((s',b')::xs) s b = if s = s' then ((s',b)::xs) else ((s',b') :: set xs s b)
				     
    (* value_of
    TYPE:(''a * bool) list -> ''a -> bool
    PRE: --
    POST: returns value of given string (key), false if not found
    SIDE EFFECTS: --
    EXAMPLE: value_of [("All monkeys fly", false),("All dogs bark",true)] "All monkeys fly" = false 
    *)
    (*VARIANT: string (key)*)
    fun value_of [] s = false
    | value_of ((s',b) :: xs) s = if s = s' then b else value_of xs s

    (* variables
    TYPE: ('a * 'b) list -> 'a list
    PRE: --
    POST: returns a list of all strings (keys) saved
    SIDE EFFECTS:
    EXAMPLE: variable [("All monkeys fly", false),("All dogs bark",true),("Some chickens swim",false)] = ["All monkeys fly","All dogs bark","Some chickens swim"]
             variable [] = []
    *)
    (*VARIANT: dictionary*)
    fun variables [] = [] 
      | variables ((s,b) :: xs) = s :: variables xs				     

    (*NOTE: This function was copied verbatim from the assignment instructions*)
    fun print valuation =
      (
	List.app (fn name => TextIO.print (name ^ " = " ^ Bool.toString (value_of valuation name) ^ "\n")) (variables valuation);
	TextIO.print "\n"
      )
end

(*2*)
(* Time COMPLEXITY:
   set: O(1)
   value_of: O(n)
   variables: O(n)
*)

(*Exercise 3*)
(*NOTE: These datatype and signature were copied verbatim from the assignment instructions. For the rest, I used the following reference as an inspiration for my answer: http://answers.google.com/answers/threadview/id/119547.html*)

datatype formula = True
		 | False
		 | Var of string
		 | Not of formula
		 | And of formula * formula
		 | Or of formula * formula


(*signature LOGIC = // This part did not work for some reason so I had to comment it out
sig
    val truth_value : T -> formula -> bool
    val is_taut : formula -> bool
end*)

functor Semantics(V: VALUATION)(*:> LOGIC*) =
struct

    (*truth_value
    TYPE: formula -> bool
    PRE: --
    POST: returns boolean value to formula
    SIDE EFFECTS: --
    EXAMPLE: truth_value v True = true
             truth_value v ("All dogs fly") = false
             truth_value v (Not("All dogs fly")) = true

    *)
    (*VARIANT: formula*)
    fun truth_value v True = true
      | truth_value v False = false
      | truth_value v (Var(x)) = V.value_of v x
      | truth_value v (Not(x)) = not (truth_value v x)
      | truth_value v (And(x, y)) = (truth_value v x) andalso (truth_value v y)
      | truth_value v (Or(x, y)) =  (truth_value v x) orelse (truth_value v y)

    (* is_taut
    TYPE: VT -> formula -> bool
    PRE: --
    POST: returns true for tautolgy otherwise false
    SIDE EFFECTS: --
    EXAMPLE: is_taut (Or (Not (Var "Some crocodiles bark"), (Var "Some crocodiles bark"))) = true
	     is_taut (And (Not (Var "Some crocodiles bark"), (Var "Some crocodiles bark"))) = false
    *)
    (*VARIANT: formula*)								
    fun is_taut f =
      let
         fun NNF (Not(Not p))= NNF p
	   | NNF (Not (And(p,q))) = NNF (Or (Not p, Not q))
	   | NNF (Not (Or(p,q))) = NNF (And (Not p, Not q))
	   | NNF (And(p,q)) = And(NNF p, NNF q)
	   | NNF (Or(p,q)) = Or(NNF p, NNF q)
	   | NNF (Not p) = Not (NNF p)
	   | NNF (p) = p;

	  fun CNF (And(p,q)) = And (CNF p, CNF q)
	    | CNF (Or(p,And(q,r))) = And(CNF(Or(p,q)), CNF(Or(p,r)))
	    | CNF (Or(And(q,r),p)) = And (CNF (Or (q,p)), CNF (Or(r,p)))
	    | CNF p = p;

	  fun atoms (Var a) = [a]
	    | atoms (Or(p,q)) = atoms p @ atoms q
	    | atoms _ = [];

	  fun negations (Not(Var a)) = [a]
	    | negations (Or(p,q)) = negations p @ negations q
	    | negations _ = [];

	  fun intersects(_,[]) = false
	    | intersects([],_) = false
	    | intersects(p,q) = ((hd p)=(hd q)) orelse intersects (p,tl q) orelse intersects (tl p, q);

	  fun tautology (And(p,q)) = tautology p andalso tautology q
	    | tautology p = if p = True then true else
	         	    if p = False then false else intersects(atoms p, negations p);

	  fun checker(p) = tautology (CNF (NNF p));
	in
	     checker f
	end
end;


(*Exercise 4*)
(*NOTE: I used the following reference as an inspiration for my answer: https://stackoverflow.com/questions/8361878/logic-simplification-in-sml*)

(* simp
TYPE: formula -> formula
PRE: --
POST: returns simplified formula
SIDE EFFECTS: --
EXAMPLE: simp Or(And(True,True),True) = True
         simp Not(Not(Not(True))) = False
         simp And(Or(And(True,True),True),Or(And(True,True),True))= True
         simp And(Or(Not(Not(True))),Not(Not(Not(True)))) = False
         simp Not(And(Or(Not(Not(True))),Not(Not(Not(True))))) = True
*)
(*VARIANT: formula*)
fun simp True = True
  | simp False = False
  | simp (Var x) = Var x
  | simp (Not x) = (case simp x of True => False
                                 | False => True
  		                 | Not x => simp x
                                 | x' => Not x')
  | simp (And(x, y)) = (case simp x of False => False
                                     | True => simp y
                                     | x' => (case simp y of False => False
                                                           | True => x'
                                                           | y' => And(x', y')))
  | simp (Or(x, y)) = (case simp x of True => True
                                    | False => simp y
                                    | x' => (case simp y of True => True
                                                          | False => x'
                                                          | y' => Or(x', y')))