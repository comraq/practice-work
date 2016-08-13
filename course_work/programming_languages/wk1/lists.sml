(*
 * Lists
 *)

val empty = [];

val x = [ 1, 2, 3 ];
val y = [ 1+2, (3+4) ];

val z = 5::x;
val w = 6::7::z;
val w' = [6, 7]::[z];

(* null is a function to check for empty list, 'a list -> bool *)
val isTrue = null empty;
val isFalse = null x;

val x'  = hd x;
val x'' = tl x;

(*
 * Note : 'a is pronounced alpha (greek letter)
 *
 * null : 'a list -> bool
 * hd   : 'a list -> 'a
 * tl   : 'a list -> 'a list
 *)

fun sum_list (xs : int list) =
  if null xs
  then 0
  else hd xs + sum_list(tl xs)

fun prod_list (xs : int list) =
  if null xs
  then 1
  else hd xs * prod_list(tl xs)

fun countdown (x : int) = (* countdown 7 = [7, 6, 5, 4, 3, 2, 1] *)
  if x = 0
  then []
  else x :: countdown(x - 1)

(* (int list) * (int list) -> int list *)
fun append (xs : int list, ys : int list) =
  if null xs
  then ys
  else hd xs :: append((tl xs), ys)

(* functions over pairs of lists *)
fun sum_pair_list (xs : (int * int) list) =
  if null xs
  then 0
  else #1 (hd xs) + #2 (hd xs) + sum_pair_list(tl xs)

fun firsts (xs : (int * int) list) =
  if null xs
  then []
  else #1 (hd xs) :: firsts(tl xs)

fun seconds (xs : (int * int) list) =
  if null xs
  then []
  else #2 (hd xs) :: seconds(tl xs)

fun sum_pair_list2 (xs : (int * int) list) =
  sum_list (firsts xs) + sum_list (seconds xs)

fun factorial (n : int) =
  if n = 0
  then 1
  else prod_list (countdown n)
