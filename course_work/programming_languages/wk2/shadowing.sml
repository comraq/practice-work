(* Shadowing Example *)

val a = 10
(*
 * a : int
 * a -> 10
 *)

val b = a * 2
(*
 * a -> 10
 * b -> 20
 *)

val a = 5 (* Not an assignment statement *)
(*
 * a -> 5
 * b -> 20
 *)

val c = b
(*
 * a -> 5
 * b -> 20
 * c -> 20
 *)

val d = a
(*
 * a -> 5
 * b -> 20
 * c -> 20
 * d -> 5
 *)

val a = a + 1
(*
 * b -> 20
 * c -> 20
 * d -> 5
 * a -> 6
 *)

val f = a * 2
