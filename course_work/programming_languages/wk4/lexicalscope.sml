(* First Example *)
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



(* Second Example *)
val x = 1

fun f y =
  let
    val x = y + 1
  in
    fn z => x + y + z (* take z and return 2y + 1 + z *)
  end

val x = 3 (* irrelevant *)

val g = f 4 (* return a function that adds 2(4) + 1 = 9 to its argument *)

val y = 5

val z = g 6 (* z = 15 *)


(* Third Example *)
fun f g =
let
  val x = 3 (* irrelevant *)
in
  g 2
end

val x = 4

fun h y = x + y (* add 4 to its argument 'y' *)

val z = f h (* z = 6 *)

(*
 * Why Lexical Scope:
 * - Meaning of a function does not depend on the variable names used
 * - Functions can be type-checked and reasoned about where they are defined
 * - Closures can easily store data they need
 *)

fun filter (f, [])    = []
  | filter (f, x::xs) = if f x then x::filter(f, xs) else filter(f, xs)

fun greaterThanX x = fn y => y > x (* int -> (int -> bool) *)

fun noNegatives xs = filter(greaterThanX ~1, xs)

fun allGreater (xs, n) = filter(fn x => x > n, xs)

