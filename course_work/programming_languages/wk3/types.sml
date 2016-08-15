datatype suit = Club | Diamond | Heart | Spade
datatype rank = Jack | Queen | King | Ace | Num of int

type card = suit * rank
type name_record = { student_num : int option,
                     first       : string,
                     middle      : string option,
                     last        : string }

fun is_Queen_of_Spades(c : card) =
  #1 c = Spade andalso #2 c = Queen

val c1 : card = (Diamond, Ace)
val c2 : suit * rank = (Heart, Ace)
val c3 = (Spade, Ace)

fun is_Queen_of_Spades c =
  case c of
       (Spade, Queen) => true
     | _              => false

datatype my_int_list = Empty
                     | Cons of int * my_int_list

val x = Cons(8, Cons(12, Cons(2016, Empty)))

fun append_my_list(xs, ys) =
  case xs of
       Empty        => ys
     | Cons(x, xs') => Cons(x, append_my_list(xs', ys))

fun inc_or_zero intoption =
  case intoption of
       NONE   => 0
     | SOME i => i + 1

fun sum_list xs =
  case xs of
       []     => 0
     | x::xs' => x + sum_list xs'

fun append(xs, ys) =
  case xs of
       []     => ys
     | x::xs' => x :: append(xs', ys)
