fun map (f, [])    = []
  | map (f, x::xs) = f x :: map(f, xs)

val x1 = map(fn x => x + 1, [ 4, 8, 12, 16 ])
val x2 = map(fn x => x < 0, [ 0, ~3, 100, ~1 ])
val x3 = map(hd, [ [1,2], [~3,4], [100,0], [9,~1] ])

fun filter (f, [])    = []
  | filter (f, x::xs) = if f x then x :: filter(f, xs) else filter(f, xs)

fun is_even v = (v mod 2 = 0)

fun all_even xs = filter(is_even, xs)

fun all_even_snd xs = filter(fn(_, v) => is_even v, xs)

fun double_or_triple f = (* (int -> bool) -> (int -> int) *)
  if f 7
  then fn x => 2 * x
  else fn x => 3 * x

val double = double_or_triple(fn x => x - 3 = 4)
val nine   = (double_or_triple(fn x => x = 42)) 3

(*
 * Function associates to the right:
 * - t1 ->  t2 ->  t3 -> t4
 * - t1 -> (t2 -> (t3 -> t4))
 *   - both above are the same
 *)

datatype exp = Constant of int
             | Negate   of exp
             | Add      of exp * exp
             | Multiply of exp * exp

fun true_of_all_constants (f, Constant i)       = f i
  | true_of_all_constants (f, Negate   e)       = true_of_all_constants(f, e)
  | true_of_all_constants (f, Add(e1, e2))      = true_of_all_constants(f, e1) andalso true_of_all_constants(f, e2)
  | true_of_all_constants (f, Multiply(e1, e2)) = true_of_all_constants(f, e1) andalso true_of_all_constants(f, e2)

fun all_even e = true_of_all_constants(fn x => x mod 2 = 0, e)
