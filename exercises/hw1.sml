(* 1. Write a function is_older that takes two dates and evaluates to true or false. It evaluates to true if the first argument is a date that comes before the second argument (If the two dates are the same, the result is false). *)
(* val is_older = fn : (int * int * int) * (int * int * int) -> bool *)

fun is_older (date1 : int * int * int, date2 : int * int * int) =
    if (#1 date1) < (#1 date2) then true
    else if (#1 date1) > (#1 date2) then false
    else if (#2 date1) < (#2 date2) then true
    else if (#2 date1) > (#2 date2) then false
    else if (#3 date1) < (#3 date2) then true
    else if (#3 date1) > (#3 date2) then false
    else false 
    
(* 2. Write a function number_in_month that takes a list of dates and a month (i.e., an int) and returns how many dates in the list are in the given month. *)
(* val number_in_month = fn : (int * int * int) list * int -> int *)
	     
fun number_in_month (ds : (int * int * int) list, month : int) =
    if null ds then 0
    else if #2 (hd ds) = month then number_in_month(tl ds, month) + 1
    else number_in_month(tl ds, month)
					     
(* 3. Write a function number_in_months that takes a list of dates and a list of months (i.e., and int list) and returns the number of dates in the list of dates that are in any of the months in the list of months. Assume the list of months has no number repeated. Hint: Use your answer to the previous problem. *)
(* fn : (int * int * int) list * int list -> int *)	

fun number_in_months (ds : (int * int * int) list, ms : int list) =
    if null ms then 0
    else number_in_month(ds, hd ms) + number_in_months(ds, tl ms)
 
(* 4. Write a function dates_in_month that takes a list of dates and a month (i.e., an int) and returns a list holding the dates from the argument list of dates that are in the month. The returned list should contain dates in the order they were originally given. *)
(* val dates_in_month = fn : (int * int * int) list * int -> (int * int * int) list *)

fun dates_in_month (ds : (int * int * int) list, month : int) =
    if null ds then []
    else if #2 (hd ds) = month then (hd ds) :: dates_in_month(tl ds, month)
    else dates_in_month(tl ds, month)

(* 5. Write a function dates_in_months that takes a list of dates and a list of months (i.e., an int list) and returns a list holding the dates from the argument list of dates that are in any of the months in the list of months. Assume the list of months has no number repeated. Hint: Use your answer to the previous problem and SML’s list-append operator (@). *)
(* val dates_in_months = fn : (int * int * int) list * int list -> (int * int * int) list *)
		       
fun dates_in_months (ds: (int * int * int) list, ms : int list) =
    if null ms then []
    else dates_in_month(ds, hd ms) @ dates_in_months(ds, tl ms)

(* 6. Write a function get_nth that takes a list of strings and an int n and returns the nth element of the list where the head of the list is 1st. Do not worry about the case where the list has too few elements: your function may apply hd or tl to the empty list in this case, which is okay. *)
(* val get_nth = fn : string list * int -> string *)						 
	
fun get_nth (strings: string list, n: int) =
    if n = 1 then hd strings
    else get_nth(tl strings, n - 1)
