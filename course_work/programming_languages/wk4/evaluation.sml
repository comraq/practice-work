(*
 * When Things Evaluate:
 *
 * - A function body is not evaluated until the function is called
 * - A function body is evaluated every time the function is called
 * - A variable binding evaluates its expression when the bidning is evaluated,
 *   not ever time the variable is used
 *
 * - Closures can help avoid repeating computations that do not depend on
 *   function arguments
 *)

fun filter (f, [])    = []
  | filter (f, x::xs) = if f x then x::filter(f, xs) else filter(f, xs)

fun allShorterThan1 (xs, s) = (* string list * string -> string list *)
  filter(fn x => String.size x < String.size s, xs)

fun allShorterThan2 (xs, s) =
let val i = String.size s
in
  filter(fn x => String.size x < i, xs)
end

(*
 * e1; e2
 *
 * Executes expression e1, discards its return value then executes e2
 *)
