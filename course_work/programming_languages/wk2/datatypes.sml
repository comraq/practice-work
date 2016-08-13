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
