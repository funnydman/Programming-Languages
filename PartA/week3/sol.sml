(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

fun all_except_option (s, xs) =
    case xs of
	[] => NONE
    | x::xs' => if same_string(x, s)
        then SOME xs'
        else case all_except_option(s, xs') of
              NONE => NONE
            | SOME tail => SOME (x::tail)


fun get_substitutions1(xs, s) =
  case xs of
       [] => []
     | x::xs' => case all_except_option(s, x) of
                      NONE => get_substitutions1(xs',s)
                    | SOME ls => ls @ get_substitutions1(xs', s)

fun get_substitutions2(xs, s) =
  let fun aux(xs, s, acc) =
  case xs of
       [] => acc
     | x::xs' => case all_except_option(s, x) of
                      NONE => aux(xs', s, acc)
                    | SOME t => aux(xs', s, t @ acc)
  in
    aux(xs, s, [])
  end

fun similar_names(xs, full_name) =
  let
  val {first=f, middle=m, last=l} = full_name
    fun helper (xs) =
      case xs of
           [] => [] |
           x::xs' => {first=x, middle=m, last=l}::(helper(xs'))


  in
    full_name::helper(get_substitutions2(xs, f))
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
fun card_color (suit, rank) =
  case suit of
       Clubs => Black |
       Spades => Black |
       Diamonds => Red |
       Hearts => Red

fun card_value (suit, rank) =
  case rank of
    Num(i) => i |
    Ace => 11 |
    _ => 10

fun remove_card (cs, c, e) =
  case cs of
       [] => raise e |
       x::xs' => if x = c then xs' else x::remove_card(xs', c, e)

fun nondecreasing xs = (* int list -> bool *)
   case xs of
      [] => true
    | _::[] => true
    | head::(neck::rest) => head <= neck andalso nondecreasing (neck::rest)

fun all_same_color(cards) =
    case cards of
        [] => true |
        _::[] => true |
        head::(neck::rest) => card_color(head) = card_color(neck) andalso all_same_color(neck::rest)

fun sum_cards (cards) =
  let fun helper(cards, acc) =
    case cards of
        [] => acc |
        x::xs' => helper(xs', card_value x + acc)
  in
    helper(cards, 0)
  end

fun primary_score (sum, goal) =
    if sum > goal then 3*(sum-goal)
    else (goal - sum)

fun score (cards, goal) =
  let val sum = sum_cards (cards)
  fun primary_score (sum, goal) =
      if sum > goal then 3 * (sum - goal)
      else (goal - sum)

  in
    if all_same_color(cards)
    then primary_score(sum, goal) div 2
    else primary_score(sum, goal)
  end

fun officiate (card_list, moves, goal) =
    let
        fun play(card_list, current_helds, remain_moves) =
            case remain_moves of
               [] => current_helds
             | head::tail => case head of
                Discard c => play(card_list, remove_card(current_helds, c, IllegalMove), tail)
              | Draw => case card_list of
                 [] => current_helds
               | c::cs =>
                    if sum_cards (c::current_helds) > goal
                    then c::current_helds
                    else play(cs, c::current_helds, tail)
    in
        score (play(card_list,[], moves), goal)
    end


