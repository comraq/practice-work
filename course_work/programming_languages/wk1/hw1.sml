fun is_older(x : int * int * int, y : int * int * int) =
let
  val xs = [ #1 x, #2 x, #3 x ]
  val ys = [ #1 y, #2 y, #3 y ]

  fun smaller(xs : int list, ys : int list) =
    if null xs
    then false
    else
      let
        val x = hd xs
        val y = hd ys
      in
        if x <> y
        then x < y
        else smaller (tl xs, tl ys)
      end

in
  smaller(xs, ys)
end

fun count(f : (int * int * int) * 'a -> 'a, ds : (int * int * int) list, m : int, c : 'a) =
let
  fun count'(ds : (int * int * int) list, c : 'a) =
    if null ds
    then c
    else
      if #2 (hd ds) = m
      then count'(tl ds, f ((hd ds), c))
      else count'(tl ds, c)
in
  count'(ds, c)
end

fun number_in_month(ds : (int * int * int) list, m : int) =
let
  fun add1(d : (int * int * int), a : int) =
    a + 1
in
  count(add1, ds, m, 0)
end

fun number_in_months(ds : (int * int * int) list, ms : int list) =
  if null ms
  then 0
  else number_in_month(ds, hd ms) + number_in_months(ds, tl ms)

fun dates_in_month(ds : (int * int * int) list, m : int) =
let
  fun concat(d : (int * int * int), ds' : (int * int * int) list) =
    ds' @ [d]
in
  count(concat, ds, m, [])
end

fun dates_in_months(ds : (int * int * int) list, ms : int list) =
  if null ms
  then []
  else dates_in_month(ds, hd ms) @ dates_in_months(ds, tl ms)

fun last(xs : 'a list) =
  if null(tl xs)
  then hd xs
  else last(tl xs)

fun take(n : int, xs : 'a list) =
  if null xs
  then []
  else
    if n = 1
    then [hd xs]
    else hd xs :: take(n - 1, tl xs)

fun take_while(f : 'a -> bool, xs : 'a list) =
  if null xs
  then []
  else
    if f (hd xs)
    then hd xs :: take_while(f, tl xs)
    else []

fun drop(n : int, xs : 'a list) =
  if null xs
  then []
  else
    if n = 1
    then tl xs
    else drop(n - 1, tl xs)

fun get_nth(xs : 'a list, n : int) =
  hd(drop(n - 1, xs))

val months = [
  "January",
  "February",
  "March",
  "April",
  "May",
  "June",
  "July",
  "August",
  "September",
  "October",
  "November",
  "December"
]

fun date_to_string(d : (int * int * int)) =
let
  val year  = #1 d
  val month = #2 d
  val day   = #3 d
in
  get_nth(months, month) ^ " " ^ Int.toString day ^ ", " ^ Int.toString(#1 d)
end

fun number_before_reaching_sum(sum : int, xs : int list) =
let
  fun smaller(x : int, pr : (int * int list)) =
  let
    val acc = #1 pr
    val xs  = #2 pr
  in
    if x + acc < sum
    then (x + acc, x :: xs)
    else (sum, xs)
  end
in
  let
    val pr = foldl smaller (0, []) xs
  in
    length(#2 pr)
  end
end

val month_days = [
  31,
  28,
  31,
  30,
  31,
  30,
  31,
  31,
  30,
  31,
  30,
  31
]

fun what_month(day : int) =
  1 + number_before_reaching_sum(day, month_days)

fun month_range(day1 : int, day2 : int) =
let
  fun range(d1 : int, ms : int list) =
    if d1 > day2
    then ms
    else
      what_month d1 :: range(d1 + 1, ms)
in
  range(day1, [])
end

fun oldest(dates : (int * int * int) list) : (int * int * int) option =
  if null dates
  then NONE
  else
    if length dates = 1
    then SOME(hd dates)
    else
      let
        val d1 = hd dates
        val d2 = hd (tl dates)
      in
        if is_older(d1, d2)
        then oldest(d1 :: tl (tl dates))
        else oldest(tl dates)
      end

fun dedupe(xs : int list) : int list =
let
  fun reducer(x' : int, xs' : int list) : int list =
    if List.exists (fn x => x = x') xs'
    then xs'
    else xs' @ [x']
in
  foldl reducer [] xs
end

fun number_in_months_challenge(ds : (int * int * int) list, ms : int list) =
  number_in_months(ds, dedupe ms)

fun dates_in_months_challenge(ds : (int * int * int) list, ms : int list) =
  dates_in_months(ds, dedupe ms)

fun reasonable_date(date : (int * int * int)) : string option =
let
  val year  = #1 date
  val month = #2 date
  val day   = #3 date
in
  if year <= 0 orelse month < 1 orelse month > 12 orelse day <= 0
  then NONE
  else
    let
      fun is_leap(year : int) =
        (year mod 400) = 0 orelse ((year mod 4) = 0 andalso (year mod 100) <> 0)

      val m_days = if is_leap year andalso month = 2
                     then 29
                     else get_nth(month_days, month)
    in
      if day > m_days
      then NONE
      else SOME (date_to_string(year, month, day))
    end
end
