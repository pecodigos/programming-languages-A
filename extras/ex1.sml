(* 1. Write a function `alternate : int list -> int` that takes a list of numbers and adds them with alternating sign. For example: `alternate [1,2,3,4] = 1 - 2 + 3 - 4 = -2`. *)

fun alternate (ns: int list) =
    let
	fun signs (count: int, ns: int list) =
	    if null (tl ns) then count
	    else if count >= 0 then signs(count - hd(tl ns), tl ns)
	    else signs(count + hd(tl ns), tl ns)
    in
	signs(hd ns, ns)
    end
	
(* 2. Write a function `min_max : int list -> int * int` that takes a non-empty list of numbers, and returns a pair `(min, max)` of the minimum and maximum of the numbers in the list. *)

fun min_max (ns: int list) =
    let
	fun tuple (n1: int, n2: int, ns: int list) =
	    if null (tl ns) then (n1, n2)
	    else if n1 < hd(tl ns) then tuple(n1, hd(tl ns), tl ns)
	    else tuple(hd(tl ns), n1, tl ns)
    in
	tuple(hd ns, hd ns, ns)
    end
	
(* 3. Write a function `cumsum : int list -> int list` that takes a list of numbers and returns a list of the partial sums of those numbers. For example: `cumsum [1,4,20] = [1,5,25]`. *)

fun cumsum (ns: int list) =
    let
	fun summation (n: int, ns: int list) =
	    if null (tl ns) then [n]
	    else n :: summation(n + hd(tl ns), tl ns)
    in
	summation(hd ns, ns)
    end
	
(* 4. Write a function `greeting : string option -> string` that given a string option `SOME name` returns the string "Hello there, ...!" where the dots would be replaced by name. Note that the name is given as an option, so if it is `NONE` then replace the dots with "you". *)

fun greeting (name: string option) =
    if isSome name then "Hello there, " ^ valOf name ^ "!"
    else "Hello there, you!"
	
(* 5. Write a function `repeat : int list * int list -> int list` that given a list of integers and another list of nonnegative integers, repeats the integers in the first list according to the numbers indicated by the second list. For example: `repeat ([1,2,3], [4,0,3]) = [1,1,1,1,3,3,3]`. *)

fun repeat (is: int list  * int list) =
    let
	fun rep (n: int, is: int list * int list) =
	    if null (#2 is) then []
	    else if n > 0 then hd(#1 is) :: rep(n - 1, ((#1 is), (#2 is)))
	    else if null (tl(#2 is)) then []
	    else rep(hd(tl(#2 is)), (tl(#1 is), tl(#2 is)))
    in
	rep(hd(#2 is), ((#1 is), (#2 is)))
    end
    
(* 6. Write a function `addOpt : int option * int option -> int option` that given two "optional" integers, adds them if they are both present (returning `SOME` of their sum), or returns `NONE` if at least one of the two arguments is `NONE`. *)

fun addOpt (ops: int option * int option) =
    if isSome (#1 ops) = false orelse isSome (#2 ops) = false then NONE
    else SOME (valOf(#1 ops) + valOf(#2 ops))
	
(* 7. Write a function `addAllOpt : int option list -> int option` that given a list of "optional" integers, adds those integers that are there (i.e., adds all the `SOME i`). For example: `addAllOpt ([SOME 1, NONE, SOME 3]) = SOME 4`. If the list does not contain any `SOME i`, i.e., they are all `NONE` or the list is empty, the function should return `NONE`. *)

fun addAllOpt (ops: int option list) =
    let
	fun is_there_some (ops: int option list) =
	    if null ops then false
	    else if isSome (hd ops) then true
	    else is_there_some(tl ops)
	fun sum (ops: int option list) =
	    if null ops then 0
	    else if isSome (hd ops) = false then sum(tl ops)
	    else valOf(hd ops) + sum(tl ops) 
    in
	if is_there_some(ops) = true then SOME (sum(ops))
	else NONE
    end
			    
(* 8. Write a function `any : bool list -> bool` that given a list of booleans returns `true` if there is at least one of them that is `true`, otherwise returns `false`. (If the list is empty it should return `false` because there is no `true`.) *)

fun any (bs: bool list) =
    if null bs then false
    else if hd bs = true then true
    else any(tl bs)
	
(* 9. Write a function `all : bool list -> bool` that given a list of booleans returns `true` if all of them are `true`, otherwise returns `false`. (If the list is empty it should return `true` because there is no `false`.) *)

fun all (bs: bool list) =
    if null bs then true
    else if hd bs = false then false
    else all(tl bs)
	    
(* 10. Write a function `zip : int list * int list -> (int * int) list` that given two lists of integers creates consecutive pairs, and stops when one of the lists is empty. For example: `zip ([1,2,3], [4,6]) = [(1,4), (2,6)]`. *)

fun zip (is: int list * int list) =
    if null (#1 is) orelse null (#2 is) then []
    else (hd(#1 is), hd(#2 is)) :: zip((tl (#1 is)), (tl (#2 is))) 

(* 11. Write a function `lookup : (string * int) list * string -> int option` that takes a list of pairs `(s, i)` and also a string `s2` to look up. It then goes through the list of pairs looking for the string `s2` in the first component. If it finds a match with the corresponding number `i`, then it returns `SOME i`. If it does not, it returns `NONE`. *)

fun lookup (ps: (string * int) list, s2: string) =
    if null ps then NONE
    else if #1(hd ps) = s2 then SOME (#2(hd ps))
    else lookup(tl ps, s2)
				      
(* 12. Write a function `splitup : int list -> int list * int list` that given a list of integers creates two lists of integers, one containing the non-negative entries, the other containing the negative entries. Relative order must be preserved: All non-negative entries must appear in the same order in which they were on the original list, and similarly for the negative entries. *)

fun splitup (is: int list) =
    if null is then ([], [])
    else
	let
	    val (positives, negatives) = splitup(tl is)
	in
	    if hd is >= 0 then (hd is :: positives, negatives)
	    else (positives, hd is :: negatives)
	end
	       
(* 13. Write a version `splitAt : int list * int -> int list * int list` of the previous function that takes an extra "threshold" parameter, and uses that instead of 0 as the separating point for the two resulting lists. *)

fun splitAt (is: int list, t: int) =
    if null is then ([], [])
    else
	let
	    val (lesser, greater) = splitAt(tl is, t)
	in
	    if hd is < t then (hd is :: lesser, greater)
	    else (lesser, hd is :: greater)
	end
	    
(* 14. Write a function `isSorted : int list -> bool` that given a list of integers determines whether the list is sorted in increasing order. *)

fun isSorted (is: int list) =
    if null (tl is) then true
    else if hd is > hd(tl is) then false
    else isSorted(tl is)
	    
(* 15. Write a function `isAnySorted : int list -> bool` that given a list of integers determines whether the list is sorted in either increasing or decreasing order. *)

fun isAnySorted (is: int list) =
    let
	fun track_inc (is: int list) =
	    if null (tl is) then true
	    else if hd is < hd(tl is) then track_inc(tl is)
	    else false
	fun track_dec (is: int list) =
	    if null (tl is) then true
	    else if hd is > hd(tl is) then track_dec(tl is)
	    else false
    in
	if track_inc(is) then true
	else if track_dec(is) then true
	else false
    end
	 
(* 16. Write a function `sortedMerge : int list * int list -> int list` that takes two lists of integers that are each sorted from smallest to largest, and merges them into one sorted list. For example: `sortedMerge ([1,4,7], [5,8,9]) = [1,4,5,7,8,9]`. *)

fun sortedMerge (is: int list * int list) =
    if null (#1 is) then #2 is
    else if null (#2 is) then #1 is
    else if hd (#1 is) < hd (#2 is) then hd (#1 is) :: sortedMerge(tl(#1 is), #2 is)
    else hd (#2 is) :: sortedMerge(#1 is, tl(#2 is))
	
(* 17. Write a sorting function `qsort : int list -> int list` that works as follows: Takes the first element out, and uses it as the "threshold" for `splitAt`. It then recursively sorts the two lists produced by `splitAt`. Finally, it brings the two lists together. (Don't forget that element you took out, it needs to get back in at some point). You could use `sortedMerge` for the "bring together" part, but you do not need to as all the numbers in one list are less than all the numbers in the other.) *)
(* I'm not sure about this answer here, but that's what I have for now *)

fun qsort (is: int list) =
    if null is then []
    else sortedMerge(splitAt(is, hd is))
				  
(* 18. Write a function `divide : int list -> int list * int list` that takes a list of integers and produces two lists by alternating elements between the two lists. For example: `divide ([1,2,3,4,5,6,7]) = ([1,3,5,7], [2,4,6])`. *)

fun divide (is: int list) =
    let
	fun l_div (is: int list, i: int) =
	    if null is then ([],[])
	    else
		let
		    val (first, second) = l_div(tl is, i + 1)
		in
		    if i mod 2 = 0 then (hd is :: first, second)
		    else (first, hd is :: second)
		end
    in
	l_div(is, 0)
    end
	
(* 19. Write another sorting function `not_so_quick_sort : int list -> int list` that works as follows: Given the initial list of integers, splits it in two lists using `divide`, then recursively sorts those two lists, then merges them together with `sortedMerge`. *)

fun not_so_quick_sort (is: int list) =
    if null is then []
    else sortedMerge(divide(is))
