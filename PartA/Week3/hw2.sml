(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

(* 1.a *)

fun all_except_option(a_string : string, a_string_list : string list) =
  let
    fun concat_reversed(reversed_head_list : string list, tail_list : string list) =
      case reversed_head_list of
        [] => tail_list
      | head::tails => concat_reversed(tails, head::tail_list)
    fun aux(to_view: string list, viewed_reversed: string list) =
      case to_view of
        [] => NONE
      | head_string::tail_string_list =>
          if same_string(head_string, a_string)
          then SOME (concat_reversed(viewed_reversed, tail_string_list))
          else aux(tail_string_list, head_string::viewed_reversed)
  in aux(a_string_list, []) end;

(* 1.b *)

fun get_substitutions1(substitutions: string list list, s : string) =
  case substitutions of
    [] => []
  | first_substitution::rest =>
    case all_except_option(s, first_substitution) of
      NONE => get_substitutions1(rest, s)
    | SOME all_except => all_except @ get_substitutions1(rest, s);

(* 1.c *)

fun get_substitutions2(substitutions: string list list, s : string) =
  let
    fun aux(remaining_substitutions : string list list, result : string list) =
      case remaining_substitutions of
        [] => result
      | first_substitution::rest =>
        case all_except_option(s, first_substitution) of
          NONE => aux(rest, result)
        | SOME all_except => aux(rest, result @ all_except)
  in aux(substitutions, []) end;

(* 1.d *)

type full_name = {first:string,middle:string,last:string};

fun similar_names(substitutions: string list list, a_full_name : full_name) =
  let
    val { first = first_name, middle = middle_name, last = last_name } = a_full_name
    val substitutions_result = get_substitutions2(substitutions, first_name)
    fun aux(substituted) =
      case substituted of
        [] => []
      | head_substituted::rest => {first = head_substituted, middle = middle_name, last = last_name}::aux(rest)
  in a_full_name::aux(substitutions_result) end;

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)

(* 2.a *)

fun card_color ((Spades, _) : card) = Black
  | card_color ((Clubs, _) : card) = Black
  | card_color ((Diamonds, _) : card) = Red
  | card_color ((Hearts, _) : card) = Red;

(* 2.b *)

fun card_value ((_, Ace) : card) = 11
  | card_value ((_, Num card_number) : card) = card_number
  | card_value ((_, _) : card) = 10;

(* 2.c *)

fun remove_card (cs : card list, c : card, e : exn) =
  case cs of
    [] => raise e
  | first_card::rest =>
    if first_card = c then rest else first_card::remove_card(rest, c, e);

(* 2.d *)

fun all_same_color (cards_list : card list) =
  case cards_list of
   first_card::second_card::rest_cards =>
    if card_color(first_card) = card_color(second_card) then all_same_color(second_card::rest_cards) else false
  | _ => true;

(* 2.e *)

fun sum_cards (cards : card list) =
  let
    fun aux ([], sum) = sum
      | aux (first_card::rest_cards, sum) = aux(rest_cards, sum + card_value(first_card))
  in aux(cards, 0) end;

(* 2.f *)

fun calculate_preliminary_score (sum, goal) = if sum > goal then 3 * (sum - goal) else (goal - sum)

fun score (held_cards : card list, goal : int) =
  let
    val sum = sum_cards(held_cards)
    val preliminary_score = calculate_preliminary_score(sum, goal)
  in
    if all_same_color(held_cards) then preliminary_score div 2 else preliminary_score
  end;

(* 2.g *)

fun officiate (card_list : card list, move_list : move list, goal : int) =
  let
    fun current_state (_, [], held_cards) = score(held_cards, goal)
      | current_state (current_card_list, (Discard c)::rest_of_current_move_list, held_cards) = current_state(current_card_list, rest_of_current_move_list, remove_card(held_cards, c, IllegalMove))
      | current_state ([], Draw::_, held_cards) = score(held_cards, goal)
      | current_state (first_of_current_card_list::rest_of_current_card_list, Draw::rest_of_current_move_list, held_cards) =
          let
            val held_cards_after_drawing = first_of_current_card_list::held_cards
          in
            if sum_cards(held_cards_after_drawing) > goal then score(held_cards_after_drawing, goal)
            else current_state(rest_of_current_card_list, rest_of_current_move_list, held_cards_after_drawing)
          end
  in
    current_state (card_list, move_list, [])
  end;

(* 3.a *)

fun count_aces ([]) = 0
  | count_aces ((_, Ace)::rest_cards) = 1 + count_aces(rest_cards)
  | count_aces (_::rest_cards) = count_aces(rest_cards)

val test'' = count_aces ([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)]) = 4

fun score_challenge (held_cards : card list, goal : int) =
  let
    fun generate_goals (0) = []
      | generate_goals (n) = (goal + 10 * n)::generate_goals(n - 1)
    fun least_score ([], least_result) = least_result
      | least_score (first_adjusted_goal::rest_adjusted_goals, least_result) =
          let val another_score = score(held_cards, first_adjusted_goal)
          in if another_score < least_result then least_score(rest_adjusted_goals, another_score) else least_score(rest_adjusted_goals, least_result) end
  in least_score(generate_goals(count_aces(held_cards)), score(held_cards, goal)) end;


fun officiate_challenge (card_list : card list, move_list : move list, goal : int) =
  let
    fun current_state (_, [], held_cards) = score_challenge(held_cards, goal)
      | current_state (current_card_list, (Discard c)::rest_of_current_move_list, held_cards) = current_state(current_card_list, rest_of_current_move_list, remove_card(held_cards, c, IllegalMove))
      | current_state ([], Draw::_, held_cards) = score_challenge(held_cards, goal)
      | current_state (first_of_current_card_list::rest_of_current_card_list, Draw::rest_of_current_move_list, held_cards) =
          let
            val held_cards_after_drawing = first_of_current_card_list::held_cards
            val max_goal = goal + 10 * count_aces(held_cards_after_drawing)
          in
            if sum_cards(held_cards_after_drawing) > max_goal then score_challenge(held_cards_after_drawing, goal)
            else current_state(rest_of_current_card_list, rest_of_current_move_list, held_cards_after_drawing)
          end
  in
    current_state (card_list, move_list, [])
  end;

(* 3.b *)

fun careful_player (card_list : card list, goal : int) =
  let
    fun act (first_of_current_card_list::second_of_current_card_list::rest_of_current_card_list, held_cards) =
      if score(first_of_current_card_list::held_cards, goal) = 0 then Draw::[]
      else if score(second_of_current_card_list::held_cards, goal) = 0 then (Discard first_of_current_card_list)::Draw::[]
      else Draw::act(second_of_current_card_list::rest_of_current_card_list, first_of_current_card_list::held_cards)
      | act (first_of_current_card_list::rest_of_current_card_list, held_cards) =
      if sum_cards(first_of_current_card_list::held_cards) > goal then (Discard first_of_current_card_list)::act(rest_of_current_card_list, held_cards)
      else if goal > (sum_cards(held_cards) + 10) then Draw::act(rest_of_current_card_list, first_of_current_card_list::held_cards)
      else if score(first_of_current_card_list::held_cards, goal) = 0 then Draw::[]
      else []
      | act ([], held_cards) = if goal > (sum_cards(held_cards) + 10) then Draw::[] else []
  in act(card_list, []) end;
