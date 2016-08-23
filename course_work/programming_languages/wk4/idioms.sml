(* Function Composition *)

(*
 * ('b -> 'c) * ('a -> 'b) -> 'a -> 'c
 *
 * This is also 'o' in sml
 *)
fun compose (f, g) = fn x => f(g x)

fun sqrt_of_abs i = Math.sqrt(Real.fromInt(abs i))

val sqrt_of_abs2 = Math.sqrt o Real.fromInt o abs

(* Own infix '!>' pipe operator *)
infix !>

fun x !> f = f x

fun sqrt_of_abs i = i !> abs !> Real.fromInt !> Math.sqrt

(* val backup1 = fn : ('a -> 'b option) * ('a -> 'b) -> 'a -> 'b *)
fun backup1 (f, g) = fn x => case f x of
                                  NONE   => g x
                                | SOME y => y

(* val backup2 = fn : ('a -> 'b) * ('a -> 'b) -> 'a -> 'b *)
fun backup2 (f, g) = fn x => f x handle _ => g x




(* Currying *)

fun sorted3_tupled (x, y, z) = z >= y andalso y >= x

val t1 = sorted3_tupled(7, 9, 11)

val sorted3 = fn x => fn y => fn z => z >= y andalso y >= x

fun sorted3_2 x = fn y => fn z => z >= y andalso y >= x

val t2 = ((sorted3 7) 9) 11

val t3 = sorted3 7 9 11

fun sorted3_nicer x y z = z >= y andalso y >= x

val t4 = sorted3_nicer 7 9 11

fun fold f acc []      = []
  | fold f acc (x::xs) = fold f (f(acc, x)) xs

fun sum xs = fold (fn (x,y) => x + y) 0 xs

val is_nonnegative = sorted3 0 0

val sum2 = fold (fn (x,y) => x + y) 0

fun is_nonnegative_inferior x = sorted3 0 0 x

fun sum_inferior xs = fold (fn (x,y) => x + y) 0 xs

fun range i j = if i > j then [] else i :: range (i + 1) j

(* range 3 6 [3, 4, 5, 6] *)

val countup = range 1

(* countup 6 [1, 2, 3, 4, 5, 6] *)

fun exists predicate []      = false
  | exists predicate (x::xs) = predicate x orelse exists predicate xs

val no = exists (fn x => x = 7) [4, 11, 23] (* false *)

val hasZero = exists (fn x => x = 0) (* int list -> bool *)

val incrementAll = List.map (fn x => x + 1) (* int list -> int list *)

val removeZeros = List.filter (fn x => x <> 0)

val pairWithOne = List.map (fn x => (x, 1))

val incrementAndPairWithOne = List.map (fn x => (x + 1, 1))

(* val curry = fn : ('a * 'b -> 'c) -> 'a -> 'b -> 'c *)
fun curry f x y = f(x, y)

(* val uncurry = fn : ('a -> 'b -> 'c) -> 'a * 'b -> 'c *)
fun uncurry f (x, y) = f x y

fun range_tuple (i, j) = if i > j then [] else i :: range_tuple(i + 1, j)

val countup_again = (curry range_tuple) 1

fun other_curry f = fn x => fn y => f y x
