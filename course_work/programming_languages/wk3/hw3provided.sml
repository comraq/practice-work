(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

val only_capitals = List.filter (fn str => Char.isUpper(String.sub(str, 0)))

val longest_string1 = foldl (fn (x, ls) => if size x > size ls then x else ls) ""

val longest_string2 = foldl (fn (x, ls) => if size x >= size ls then x else ls) ""

(* longest_string_helper : (int * int -> bool) -> string list -> string *)
fun longest_string_helper f =
  foldl (fn (x, ls) => if f(size x, size ls) then x else ls) ""

val longest_string3 = longest_string_helper (fn (a, b) => a > b)

val longest_string4 = longest_string_helper (fn (a, b) => a >= b)

val longest_capitalized = longest_string1 o only_capitals

val rev_string = implode o rev o explode

fun first_answer f xs =
let
  fun helper (x, NONE) = f x
    | helper (_, v)    = v
in
  case foldl helper NONE xs of
       SOME v => v
     | NONE   => raise NoAnswer
end

fun all_answers f =
let
  fun helper(x, SOME xs) = (case f x of
                                 SOME ys => SOME(xs @ ys)
                               | _       => NONE)
    | helper(_, NONE)    = NONE
in
  foldl helper (SOME [])
end


datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
let
  val r = g f1 f2
in
  case p of
       Wildcard          => f1 ()
     | Variable x        => f2 x
     | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
     | ConstructorP(_,p) => r p
     | _                 => 0
end

val count_wildcards = g (fn _ => 1) (fn _ => 0)

val count_wild_and_variable_lengths = g (fn _ => 1) (fn str => size str)

fun count_some_var (str, pat) = g (fn _ => 0) (fn s => if s = str then 1 else 0) pat

val check_pat =
let
  fun get_var_names (Variable x)           = [x]
    | get_var_names (TupleP ps)            = foldl (fn (x, acc) => acc @ get_var_names x) [] ps
    | get_var_names (ConstructorP(_, pat)) = get_var_names pat
    | get_var_names _                      = []

  val has_repeats =
    foldr (fn (x : string, acc) =>
    case acc of
         (false, xs) => (false, xs)
       | (_, xs)     => (not (List.exists (fn x' => x' = x) xs), x::xs)) (true, [])

  fun fst (a, b) = a
in
  fst o has_repeats o get_var_names
end

fun match (_             , Wildcard               ) = SOME []
  | match (value         , Variable s             ) = SOME [(s, value)]
  | match (Unit          , UnitP                  ) = SOME []
  | match (Const v       , ConstP p               ) = if v = p then SOME [] else NONE
  | match (Constructor(s2, v), ConstructorP(s1, p)) = if s2 = s1 then match(v, p) else NONE
  | match (Tuple vs      , TupleP ps              ) = if length vs <> length ps
                                                      then NONE
                                                      else all_answers match (ListPair.zip(vs, ps))
  | match _                                         = NONE

fun first_match v ps =
  (SOME (first_answer (fn p => match(v, p)) ps))
  handle NoAnswer => NONE

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)

(**** typecheck_patterns : ((string * string * type) list) * (pattern list) -> typ option ****)
