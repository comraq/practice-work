(*
 * Note: Modules with the same signature define different types regardless of
 *       implementation
 *       ie: Rational1.rational <> Rational3.rational
 *)
signature RATIONAL_B =
sig
  type rational
  exception BadFrac

  val make_frac : int * int -> rational
  val add : rational * rational -> rational
  val toString : rational -> string
end

signature RATIONAL_C =
sig
  type rational (* This is an abstract type, concealing that actual constructors *)
  exception BadFrac

  val Whole : int -> rational
  val make_frac : int * int -> rational
  val add : rational * rational -> rational
  val toString : rational -> string
end

structure Rational1 :> RATIONAL_C =
struct
  (* Invariant 1: all denominators > 0 *)
  (* Invariant 2: rationals kept in reduced form *)

  datatype rational = Whole of int | Frac of int * int
  exception BadFrac

  fun gcd (x, y) =
    if x = y
    then x
    else if x < y
    then gcd(x, y - x)
    else gcd(y, x)

  fun reduce (Whole n)    = Whole n
    | reduce (Frac(x, y)) =
      if x = 0
      then Whole 0
      else let val d = gcd(abs x, y) in
             if d = y
             then Whole(x div d)
             else Frac(x div d, y div d)
           end

  fun make_frac (x, 0) = raise BadFrac
    | make_frac (x, y) =
      if y < 0
      then reduce(Frac(~x, ~y))
      else reduce(Frac(x, y))

  (* Assumes arguments are reduced *)
  fun add (Whole i, Whole j)       = Whole(i + j)
    | add (Whole i, Frac(j, k))    = Frac(j + k * i, k)
    | add (Frac(j, k), Whole i)    = Frac(j + k * i, k)
    | add (Frac(a, b), Frac(c, d)) = reduce (Frac(a * d + b * c, b * d))

  (* Assumes arguments are reduced *)
  fun toString (Whole i)    = Int.toString i
    | toString (Frac(a, b)) = (Int.toString a) ^ "/" ^ (Int.toString b)
end

structure Rational3 :> RATIONAL_B =
struct
  type rational = (int * int)
  exception BadFrac

  fun make_frac (x, 0) = raise BadFrac
    | make_frac (x, y) =
      if y < 0
      then (~x, ~y)
      else (x, y)

  fun add ((a, b), (c, d)) = (a * d + c * b, b * d)

  fun toString (0, y) = "0"
    | toString (x, y) =
      let
        fun gcd (x, y) =
          if x = y
          then x
          else if x < y
          then gcd(x, y - x)
          else gcd(y, x)

        val d = gcd(abs x, y)
        val num = x div d
        val denom = y div d
      in
        Int.toString num ^ (if denom = 1
                            then ""
                            else "/" ^ (Int.toString denom))
      end

  (*
   * Note: Function names can start with Capital letters,
   *       does not have to be constructor
   *)
  fun Whole i = (i, 1)
end
