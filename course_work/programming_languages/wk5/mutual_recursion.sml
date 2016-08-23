(*
 * f1, f2 and f3 are all bound in the same bundle,
 * can mutually call each other
 *
 * datatype declarations also supports a similar feature
 *)
fun f1 p1 = 1
and f2 p2 = 2
and f3 p3 = 3

(* Finite State Machine Example using Mutual Recursion *)

fun match xs = (* [1, 2, 1, 2, 1, 2] *)
let
  fun s_need_one []      = true
    | s_need_one (1::xs) = s_need_two xs
    | s_need_one _       = false

  and s_need_two []      = false
    | s_need_two (2::xs) = s_need_one xs
    | s_need_two _       = false
in
  s_need_one xs
end

datatype t1 = Foo of int | Bar of t2
and t2 = Baz of string | Quux of t1

fun no_zeros_or_empty_strings_t1 (Foo i) = i <> 0
  | no_zeros_or_empty_strings_t1 (Bar y) = no_zeros_or_empty_strings_t2 y

and no_zeros_or_empty_strings_t2 (Baz s)  = size s > 0
  | no_zeros_or_empty_strings_t2 (Quux y) = no_zeros_or_empty_strings_t1 y


(* Mutual Recursion using Higher Order Functions *)
fun no_zeros_or_empty_strings_t1_alternate (f, Foo i) = i <> 0 (* f : t2 -> bool *)
  | no_zeros_or_empty_strings_t1_alternate (f, Bar y) = f y

and no_zeros_or_empty_strings_t2_alternate (Baz s)  = size s > 0
  | no_zeros_or_empty_strings_t2_alternate (Quux y) = no_zeros_or_empty_strings_t1_alternate(no_zeros_or_empty_strings_t2_alternate, y)
