(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

fun all_except (check : 'a -> bool, xs : 'a list) : 'a list option =
let
  fun helper []      = NONE
    | helper (x::xs) =
      if check(x)
      then SOME xs
      else case helper(xs) of
                SOME xs' => SOME(x::xs')
              | _        => NONE
in
  helper(xs)
end

(* put your solutions for problem 1 here *)
fun all_except_option (s1 : string, s2 : string list) : string list option =
let
  fun is_s1(s1' : string) =
    same_string(s1, s1')
in
  all_except(is_s1, s2)
end

(* string list list * string -> string list *)
fun get_substitutions1 ([], s)      = []
  | get_substitutions1 (xs::xss, s) =
      case all_except_option(s, xs) of
           SOME ys => ys @ get_substitutions1(xss, s)
         | _       => [] @ get_substitutions1(xss, s)

fun get_substitutions2 (xss, s) =
let
  fun helper(acc, []     , s) = acc
    | helper(acc, xs::xss, s) =
    case all_except_option(s, xs) of
         SOME ys => helper(acc @ ys, xss, s)
       | _       => helper(acc, xss, s)
in
  helper([], xss, s)
end

fun similar_names (xss : string list list, { first=f, middle=m, last=l }) =
let
  val replace_fs = get_substitutions2(xss, f)
  fun helper([])   = []
    | helper(x::xs) = { first=x, middle=m, last=l } :: helper(xs)
in
  {first=f, middle=m, last=l} :: helper(replace_fs)
end


(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)

fun card_color (Clubs,  _)   = Black
  | card_color (Spades, _)   = Black
  | card_color (_, _)        = Red

fun card_value (_, Ace)      = 11
  | card_value (_, Num i)    = i
  | card_value (_, _)        = 10

fun remove_card (cs : card list, c : card, e : exn) : card list =
let
  fun is_card(c' : card) : bool =
    c' = c
in
  case all_except(is_card, cs) of
       SOME cs' => cs'
     | _        => raise e
end

fun all_same_color ([]         : card list) : bool = true
  | all_same_color (c::[]      : card list) : bool = true
  | all_same_color (c1::c2::cs : card list) : bool =
    card_color c1  = card_color c2 andalso all_same_color(cs)

fun sum_cards (cards : card list) : int =
let
  fun helper(value, [])    = value
    | helper(value, c::cs) = helper(value + card_value(c), cs)
in
  helper(0, cards)
end

fun score (held_cards : card list, goal : int) : int =
let
  val sum_values   = sum_cards(held_cards)
  val prelim_score = if sum_values > goal
                     then 3 * (sum_values - goal)
                     else goal - sum_values
in
  if all_same_color(held_cards)
  then prelim_score div 2
  else prelim_score
end

fun get_end_game_hand (deck : card list, moves : move list, goal : int) : card list =
let
  val start_cards = []
  fun make_move(deck,    Discard c::ms, hand) = make_move(deck, ms, remove_card(hand, c, IllegalMove))
    | make_move(deck,    [],            hand) = hand
    | make_move([],      _,             hand) = hand
    | make_move(c::deck, Draw::ms,      hand) =
      let
        val new_hand = c::hand
      in
        if sum_cards(new_hand) > goal
        then new_hand
        else make_move(deck, ms, new_hand)
      end
in
  make_move(deck, moves, start_cards)
end

fun officiate (deck : card list, moves : move list, goal : int) : int =
  score( get_end_game_hand(deck, moves, goal), goal )

fun score_challenge (held_cards : card list, goal : int) : int =
let
  fun is_Ace (_, Ace) = true
    | is_Ace (_, _  ) = false

  fun non_neg_min (i : int, j : int) : int =
    if i < 0
    then j
    else if j < 0 then i else Int.min(i, j)

  fun helper (min_score : int, cards : card list) : int =
    case all_except(is_Ace, cards) of
         NONE        => non_neg_min(min_score, score(cards, goal))
       | SOME cards' => helper( non_neg_min(min_score, 1 + score(cards', goal)), cards' )
in
  helper (~1, held_cards)
end

fun officate_challenge (deck : card list, moves : move list, goal : int) : int =
  score_challenge( get_end_game_hand(deck, moves, goal), goal)

fun careful_player (cards : card list, goal : int) : move list =
let
  fun card_with_value_in_list(value : int, cards : card list) : card option =
  let
    fun is_value(card : card) : bool =
      card_value card = value

    fun helper([]      ) = NONE
      | helper(c::cards) =
        if is_value c then SOME c else helper cards
  in
    helper cards
  end


  fun helper([],      moves   , _   ) = moves
    | helper(d::deck, []      , hand) = helper(deck, [Draw]     , d::hand)
    | helper(d::deck, moves   , []  ) = helper(deck, Draw::moves, [d]    )
    | helper(d::deck, m::moves, hand) =
      let
        val hand_value      = sum_cards(hand)
      in
        if hand_value > goal        then moves else
        if goal - hand_value > 10   then helper(deck, Draw::m::moves, d::hand) else
        if score(hand, goal) = 0 then moves else
          let
            val next_card_value  = card_value d
            val value_to_discard = hand_value + next_card_value - goal
          in
            if value_to_discard < 0
            then helper(deck, Draw::moves, d::hand)
            else case card_with_value_in_list(value_to_discard, hand) of
                      NONE   => helper(deck, Draw::moves, d::hand)
                    | SOME c => (Discard c)::Draw::moves
          end
      end
in
  helper(cards, [], [])
end
