(* This is a comment. This is our first program. *)

val x = 34;
(* static environment: x : int *)
(* dynamic environment: x --> 34 *)

val y = 17;
(* static environment: x : int, y : int *)
(* dynamic environment: x --> 34, y --> 17 *)

val z = (x + y) + (y + 2);
(* static environment: x : int, y : int, z : int *)
(* dynamic environment: x --> 34, y --> 17, z --> 70 *)

val w = z + 1;
(* static environment: x : int, y : int, z : int, w : int *)
(* dynamic environment: x --> 34, y --> 17, z --> 70, w --> 71 *)

val abs_of_z = if z < 0 then 0 - z else z; (* bool *) (* int *)
(* abs_of_z : int *)
(* dynamic environment : ..., abs_of_z --> z *)

val abs_of_z_simpler = abs z;

val test_of_int = if z > 0 then z - 20 else z;
