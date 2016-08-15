exception ListLengthMismatch

fun old_zip3(l1, l2, l3) =
  if null l1 andalso null l2 andalso null l3
  then []
  else if null l1 orelse null l2 orelse null l3
  then raise ListLengthMismatch
  else (hd l1, hd l2, hd l3) :: old_zip3(tl l1, tl l2, tl l3)

fun zip3 list_triple =
  case list_triple of
       ([], [], [])          => []
     | (x::xs, y::ys, z::zs) => (x, y, z) :: zip3(xs, ys, zs)
     | _                     => raise ListLengthMismatch

fun unzip3 lst =
  case lst of
       []            => ([], [], [])
     | (a, b, c)::xs => let val (l1, l2, l3) = unzip3 xs
                        in
                          (a::l1, b::l2, c::l3)
                        end

fun bad_nondecreasing xs = (* int list -> bool *)
  case xs of
       [] => true
     | x::xs' => case xs' of
                      [] => true
                    | y::ys' => x <= y andalso bad_nondecreasing xs'

fun nondecreasing xs = (* int list -> bool *)
  case xs of
       []                 => true
     | _::[]              => true
     | head::(neck::rest) => head <= neck andalso nondecreasing (neck::rest)

datatype sgn = P | N | Z

fun multsign(x1, x2) = (* int * int -> sgn *)
let fun sign x = if x = 0 then Z else if x > 0 then P else N
in
  case (sign x1, sign x2) of
       (Z, _) => Z
     | (_, Z) => Z
     | (P, P) => P
     | (N, N) => P
     | _      => N
end

fun len xs =
  case xs of
       []     => 0
     | _::xs' => 1 + len xs'
