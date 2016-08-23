(*
 * f: T1 -> T2 [must be a function as all functions take 1 arg]
 * x: T1
 *
 * y: T3
 * z: T4
 *
 * T1 = T3 * T4 [else pattern match does not type-check]
 * T3 = int [as abs has type int -> int]
 * T4 = int [because z is added to an int]
 *
 * so T1 = int * int
 * so (abs y) + z : int, so let-expression : int, so body : int
 * T2 = int
 *
 * f : int * int -> int
 *)
fun f x =
let val (y, z) = x in
  (abs y) + z
end

(*
 * sum : T1 -> T2
 *
 * x : T3
 * xs : T3 list [pattern match a T1]
 *
 * T1 = T3 list
 * T2 = int [because 0 might be returned]
 * T3 = int [because x : T3 and we add x to something]
 * from T1 = T3 list and T3 = int, we know T1 = int list
 * from that and T2 = int, we know f : int list -> int
 *)
fun sum []      = 0
  | sum (x::xs) = x + (sum xs)

(*
 * length : T1 -> T2
 * xs : T1
 *
 * x : T3
 * xs : T3 list
 *
 * T1 = T3 list
 * T2 = int
 *
 * T3 list -> int
 * 'a list -> int (replace unconstrained types with generic type)
 *)
fun length []      = 0
  | length (x::xs) = 1 + length xs

(*
 * f: T1 * T2 * T3 -> T4
 * x: T1
 * y: T2
 * z: T3
 *
 * T4 = T1 * T2 * T3
 * T4 = T2 * T1 * T3
 * - Type checker does not know which branch of boolean will fall through, thus
 *   both type definitions of T4 must be true
 *   - only way both of the above can be true is if T1 = T2
 *
 * f : T1 * T1 * T3 -> T1 * T1 * T3
 * f : 'a * 'a * 'b -> 'a * 'a * 'b
 *)
fun f (x, y, z) =
  if true
  then (x, y, z)
  else (y, x, z)

(*
 * compose : T1 * T2 -> T3
 * f : T1
 * g : T2
 * x : T4
 *
 * body being a function has T3 = T4 -> T5
 * from g being passed x, T2 = T4 -> T6 for some T6
 * from f being passed the result of g, T1 = T6 -> T7
 * from call to f being the body of anonymous function T7 = T5
 *
 * T1 = T6 -> T5
 * T2 = T4 -> T6
 * T3 = T4 -> T5
 *
 * (T6 -> T5) * (T4 -> T6) -> (T4 -> T5)
 * ('a -> 'b) * ('c -> 'a) -> 'c -> 'b
 *)
fun compose (f, g) = fn x => f (g x)


(* Problem *)
val r = ref NONE (* val r : 'a option ref *)
(*
val _ = r := SOME "hi"
val i = 1 + valOf (!r)
 *)

(* Fix *)
val r = ref NONE (* val r : ?.X1 option ref *)
(*
val _ = r := SOME "hi"
val i = 1 + valOf (!r)
 *)

(* val pairWithOne = List.map (fn x => (x, 1)) *)
(* does not get type 'a list -> ('a * int) list *)

fun pairWithOne2 xs = List.map (fn x => (x, 1)) xs
