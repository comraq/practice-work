(* Callbacks *)

(* val onKeyEvent : (int -> unit) -> unit *)

val cbs : (int -> unit) list ref = ref []

fun onKeyEvent f = cbs := f :: (!cbs)

fun onEvent i =
let
  fun loop []      = ()
    | loop (f::fs) = (f i; loop fs)
in
  loop (!cbs)
end

val timesPressed = ref 0
val _ = onKeyEvent (fn _ => timesPressed := (!timesPressed) + 1)

fun printIfPressed i =
  onKeyEvent (fn j =>
    if i = j
    then print ("You Pressed " ^ Int.toString i ^ "\n")
    else ())

val _ = printIfPressed 4
val _ = printIfPressed 11
val _ = printIfPressed 23
val _ = printIfPressed 4
