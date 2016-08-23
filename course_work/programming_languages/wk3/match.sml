fun sum_triple triple =
  case triple of
       (x, y, z) => x + y + z

fun full_name r =
  case r of
       { first = x, middle = y, last = z} => x ^ " " ^ y ^ " " ^ z

fun sum_triple2 triple =
let val (x, y, z) = triple
in
  x + y + z
end

fun full_name2 r =
let val { first = x, middle = y, last = z } = r
in
  x ^ " " ^ y ^ " " ^ z
end

fun sum_triple3 (x, y, z) =
  x + y + z

fun full_name3 { first=x, middle=y, last=z } =
  x ^ " " ^ y ^ " " ^ z

fun rotate_left(x, y, z) = (y, z, x)

fun rotate_right t = rotate_left(rotate_left t)

fun partial_sum(x, y, z) =
  x + z

fun partial_name { first=x, middle=y, last=z } =
  x ^ " " ^ z

datatype exp = Constant of int
             | Negate   of exp
             | Add      of exp * exp
             | Multiply of exp * exp

fun eval (Constant i)        = i
  | eval (Negate e2)         = ~ (eval e2)
  | eval (Add (e1, e2))      = eval e1 + eval e2
  | eval (Multiply (e1, e2)) = eval e1 * eval e2

fun append ([], ys)    = ys
  | append (x::xs, ys) = x :: append(xs, ys)

