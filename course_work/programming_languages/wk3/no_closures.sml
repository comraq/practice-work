datatype 'a mylist = Cons of 'a * ('a mylist)
                   | Empty

fun map f Empty         = Empty
  | map f (Cons(x, xs)) = Cons(f x, map f xs)

fun filter f Empty         = Empty
  | filter f (Cons(x, xs)) = if f x then Cons(x, filter f xs) else filter f xs

fun length Empty         = 0
  | length (Cons(_, xs)) = 1 + length xs

val doubleAll = map (fn x => x * 2)

fun countNs (xs, n : int) = length (filter (fn x => x = n) xs)
