(*

Two functions are equivalent if they have the same "observable behaviour",
no matter how they are used anywhere in any program.

Given equivalent arguments, they:
- Produce equivalent results
- Have the same (non-)termination behaviour
- Mutate (non-local) memory in the same way
- Do the same input/output
- Raise the same exceptions

Notice it is much easier to be equivalent if:
- There are fewer possible arguments
  - ex: with a type system and abstraction
- Avoid side-effects: mutation, input/output, exceptions and etc...
  - this is reinforced in functional languages

***************
fun f x = x + x

  'equals'

val y = 2
fun f x = y * x
***************

****************************
fun g (f, x) = (f x) + (f x)

  'not equals' (because f may have side effects)

val y = 2
fun g (f, x) = y * (f x)
****************************

*************
fun f x =
let
  val y = g x
  val z = h x
in
  (y, z)
end

  'not equals' (because both g or h may have side effects)

fun f x =
let
  val z = h x
  val y = g x
in
  (y, z)
end
*************

fun f x = x andalso g x '===' fun f x = if x then g x else false
fun f x = x andalso g x '=/=' fun f x = if g x then x else false

val y = 14
fun f x = x + y + x '===' fun f z = z + y + z
fun f x = x + y + x '=/=' fun f y = y + y + y

fun f x = let val y = 3 in x + y end '=/=' fun f y = let val y = 3 in y + y end

fun g z = (z + y + z) + z '===' fun f x = x + y + x; fun g z = (f z) + z

val y = 14; val y = 7; fun g z = (z + y + z) + z '=/=' val y = 14; fun f x = x + y + x; val y = 7; fun g z = (f z) + z

fun f x = x + x; fun g y = f y '===' fun f x = x + x; val g = f

fun f x = x + x; fun h () = (print "hi"; f); fun g y = (h ()) y '=/=' fun f x = x + x; fun () = (print "hi"; f); val g = (h ())

let val x = e1 in e2 end '===' (fn x => e2) e1

 *)
