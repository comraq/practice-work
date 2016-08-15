datatype mytype = TwoInts of int * int
                | Str of string
                | Pizza

val a = Str "hi"
val b = Str
val c = Pizza
val d = TwoInts(1 + 2, 3 + 4)
val e = a

(* mytype -> int *)
fun f (x : mytype) =
  case x of
       Pizza => 3
     | Str s => String.size s
     | TwoInts(i1, i2) => i1 + i2

(*   | Pizza => 4; (* redudant case: error *) *)
(* fun g x = case x of Pizza => 3 (* missing cases: warning *) *)

datatype suit = Club | Diamond | Heart | Spade
datatype rank = Jack | Queen | King | Ace | Num of int

datatype id = StudentNum of int
            | Name       of string * (string option) * string

datatype exp = Constant of int
             | Negate   of exp
             | Add      of exp * exp
             | Multiply of exp * exp

fun eval e =
  case e of
       Constant i        => i
     | Negate e2         => ~ (eval e2)
     | Add (e1, e2)      => (eval e1) + (eval e2)
     | Multiply (e1, e2) => (eval e1) * (eval e2)

fun number_of_adds e = (* exp -> int *)
  case e of
       Constant i        => 0
     | Negate e2         => number_of_adds e2
     | Add (e1, e2)      => 1 + number_of_adds e1 + number_of_adds e2
     | Multiply (e1, e2) => number_of_adds e1 + number_of_adds e2

val example_exp : exp  = Add (Constant 19, Negate (Constant 4))

val example_ans : int = eval example_exp

val example_addcount = number_of_adds (Multiply (example_exp, example_exp))

(* exp -> int *)
fun max_constant e =
let fun max_of_two(e1, e2) =
      Int.max(max_constant e1, max_constant e2)
in
  case e of
       Constant i        => i
     | Negate e2         => max_constant e2
     | Add (e1, e2)      => max_of_two(e1, e2)
     | Multiply (e1, e2) => max_of_two(e1, e2)
end

val nineteen = max_constant example_exp
