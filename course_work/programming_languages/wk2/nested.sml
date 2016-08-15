fun countup_from1(x : int) =
let
  fun count (from : int) =
    if from = x
    then x::[]
    else from :: count(from + 1)
in
  count(1)
end

fun bad_max(xs : int list) =
  if null xs
  then 0
  else if null (tl xs)
  then hd xs
  else if hd xs > bad_max(tl xs)
  then hd xs
  else bad_max(tl xs)

(* return [from, from + 1, ... , to] *)
fun countup(from : int, to : int) =
  if from = to
  then to :: []
  else from :: countup(from + 1, to)

(* return [from, from - 1, ... , to] *)
fun countdown(from : int, to : int) =
  if from = to
  then to :: []
  else from :: countdown(from - 1, to)

fun good_max(xs : int list) =
  if null xs
  then 0
  else if null (tl xs)
  then hd xs
  else
    let
      val tl_ans = good_max(tl xs)
      val x = hd xs
    in
      if x > tl_ans
      then x
      else tl_ans
    end

(* fn : int list -> int option *)
fun new_max(xs : int list) =
  if null xs
  then NONE
  else
    let
      val tl_ans = new_max(tl xs)
      val x = hd xs
    in
      if isSome tl_ans andalso valOf tl_ans > x
      then tl_ans
      else SOME x
    end

fun new_max2(xs : int list) =
  if null xs
  then NONE
  else
    let
      (* assume argument nonempty as the following function is local *)
      (* int list -> int *)
      fun max_nonempty(xs : int list) =
        if null (tl xs) (* xs will be single element list *)
        then hd xs
        else
          let
            val tl_ans = max_nonempty(tl xs)
            val x = hd xs
          in
            if x > tl_ans
            then x
            else tl_ans
          end
    in
      SOME (max_nonempty xs)
    end
