fun double x = 2 * x
fun incr   x = x + 1

val a_tuple  = (double, incr, double(incr 7))
val eighteen = (#1 a_tuple) 9

fun increment_n_times_lame (n, x) =
  if n = 0
  then x
  else 1 + increment_n_times_lame(n - 1, x)

fun double_n_times_lame (n, x) =
  if n = 0
  then x
  else 2 * double_n_times_lame(n - 1, x)

fun nth_tail_lame (n, xs) =
  if n = 0
  then xs
  else tl (nth_tail_lame(n - 1, xs))

fun n_times (f, n, x) =
  if n = 0
  then x
  else f(n_times(f, n - 1, x))

fun increment x = x + 1
fun double x    = x * 2

val x1 = n_times(double, 4, 7)
val x2 = n_times(increment, 4, 7)
val x3 = n_times(tl, 2, [4, 8, 12, 16])

fun increment_n_times (n, x) = n_times(increment, n, x)
fun double_n_times (n, x)    = n_times(double, n, x)
fun nth_tail (n, x)          = n_times(tl, n, x)

fun triple x = x * 3
fun triple_n_times (n, x)    = n_times(triple, n, x)

fun times_until_zero (f, x) =
  if x = 0 then 0 else 1 + times_until_zero(f, f x)

fun len  []      = 0
  | len (_::xs)  = 1 + len xs

fun triple_n_times2 (n, x) =
let
  fun triple x = 3 * x
in
  n_times(triple, n, x)
end

fun triple_n_times3 (n, x) =
  n_times(let fun triple x = 3 * x in triple end, n , x)

fun triple_n_times4 (n, x) =
  n_times(fn x => 3 * x, n, x)

val triple_n_times5 = fn (n, x) => n_times(fn x => 3 * x, n, x)

val rev = List.rev
