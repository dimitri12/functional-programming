(* fun iota n
	Type int -> int list
	pre: n>0
	Post: a list in a order from 0 to n
*)
fun iota 0 = []
  | iota n = (iota (n-1) @ [n-1]);

fun member (x,[]) = false
  | member (x, y::ys) = x=y orelse member (x,ys);

  fun inter ([], []) = []
    | inter (s1, []) = []
    | inter ([], s2) = []
    | inter (si::s1, s2) =
        if member (si,s2) then (si::inter (s1,s2))
        else inter (s1,s2);

   fun inter' ([], []) = []
    	| inter' (s1, []) = []
    	| inter' ([], s2) = []
   		| inter' (si::s1,si2::s2) = 
    
	    if si<si2 then inter'(s1, si2::s2)
		else if si=si2 then si :: inter'(s1, s2)
		else inter'(si::s1, s2);


(* real time f
TYPE: (unit −> ’a) −> Time.time ∗ ’a
PRE : true
POST: (the amount of (real) time spent evaluating f (), f ()) SIDE−EFFECTS: any side−effects caused by evaluating f ()
*)
fun real_time f =
	let
	val rt = Timer.startRealTimer() 
	val result = f ()
	val time = Timer.checkRealTimer rt
	in
	(time , result)
	end

	val result = 
	let
	val s1 = iota 100000
	val s2 = iota 1000000
	val slow_time_and_res = real_time (fn () =>inter (s1,s2)) 
	val fast_time_and_res = real_time (fn () =>inter' (s1,s2))
	in
	(slow_time_and_res,fast_time_and_res)
end