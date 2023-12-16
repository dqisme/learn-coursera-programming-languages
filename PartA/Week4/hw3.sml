(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

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

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)

fun only_capitals a_string_list =
	List.filter
		(fn a_string => Char.isUpper(String.sub(a_string, 0)))
		a_string_list

fun longest_string1 [] = ""
  | longest_string1 a_string_list =
	foldl
		(fn (head_string, longest_string) => (if (String.size head_string) > (String.size longest_string) then head_string else longest_string))
		(hd a_string_list)
		a_string_list

fun longest_string2 a_string_list =
	foldl
		(fn (head_string, longest_string) => (if (String.size head_string) >= (String.size longest_string) then head_string else longest_string))
		(hd a_string_list)
		a_string_list

fun longest_string_helper comparator a_string_list =
	foldl
		(fn (head_string, longest_string) => (if comparator(String.size head_string, String.size longest_string) then head_string else longest_string))
		(hd a_string_list)
		a_string_list

val longest_string3 = longest_string_helper (fn (one_number, other_number) => one_number > other_number)

val longest_string4 = longest_string_helper (fn (one_number, other_number) => one_number >= other_number)

val longest_capitalized = longest_string1 o only_capitals

val rev_string = implode o rev o explode

fun first_answer _ [] = raise NoAnswer
  | first_answer get_answer question_list =
	case get_answer (hd question_list) of
		SOME answer => answer
	  |	NONE => (first_answer get_answer (tl question_list))

fun all_answers get_answers question_list =
    let fun accumulator ([], result) = result
          | accumulator (remain, SOME accumulation) =
            case get_answers(hd remain) of
                NONE => NONE
              | SOME answers => accumulator(tl remain, SOME (accumulation @ answers))
    in accumulator(question_list, SOME [])
    end;

fun count_wildcards a_pattern = g (fn _ => 1) (fn _ => 0) a_pattern

fun count_wild_and_variable_lengths a_pattern = g (fn _ => 1) (fn variable_name => String.size variable_name) a_pattern

fun count_some_var (a_string, a_pattern) = g (fn _ => 0) (fn variable_name => if variable_name = a_string then 1 else 0) a_pattern

fun all_variable_names a_pattern =
    case a_pattern of
        Variable variable_name => [variable_name]
      | TupleP patterns => List.foldl (fn (current_pattern, variable_name_list) => variable_name_list @ (all_variable_names current_pattern)) [] patterns
      | ConstructorP(_, constructor_pattern) => all_variable_names constructor_pattern
      | _ => []

fun check_no_repeats [] = true
  | check_no_repeats (head::tail) = (List.all (fn current => current <> head) tail) andalso (check_no_repeats tail)
(* 
val test_check_no_repeats_should_be_true = check_no_repeats ["a", "b", "c"] = true
val test_check_no_repeats_should_be_false = check_no_repeats ["a", "b", "c", "b"] = false
 *)

val check_pat = check_no_repeats o all_variable_names
(* 
val test_check_pat_should_be_true = check_pat (TupleP [Variable "a", Variable "b", Variable "c"]) = true
val test_check_pat_should_be_false = check_pat (TupleP [Variable "a", Variable "b", Variable "b"]) = false
 *)

fun match (_, Wildcard) = SOME []
  | match (any_value, Variable variable_name) = SOME [(variable_name, any_value)]
  | match (Unit, UnitP) = SOME []
  | match (Const const_value, ConstP const_pattern) = if const_value = const_pattern then SOME [] else NONE
  | match (Tuple tuple_value_list, TupleP tuple_pattern_list) =
        if List.length tuple_value_list <> List.length tuple_pattern_list
        then NONE
        else all_answers match (ListPair.zip (tuple_value_list, tuple_pattern_list))
  | match (Constructor (constructor_value_name, constructor_value), ConstructorP (constructor_pattern_name, constructor_pattern)) =
        if constructor_value_name <> constructor_pattern_name
        then NONE
        else match (constructor_value, constructor_pattern)
  | match _ = NONE

fun first_match a_value a_pattern_list =
    SOME (first_answer (fn a_pattern => match (a_value, a_pattern)) a_pattern_list)
    handle NoAnswer => NONE