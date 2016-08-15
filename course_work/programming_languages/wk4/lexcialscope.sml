val x = 1
  (* x maps to 1 *)

fun f y = x + y
  (* f maps to a function that adds 1 to its argument *)

val x = 2
  (* shadows x and now x maps to 2 *)

val y = 3
  (* shadows y and now y maps to 3 *)

val z = f (x + y)
  (* call the function defined on line 2 with 5 *)
  (* z maps to 6 --> (1 + (2 + 3)) = 6 *)
