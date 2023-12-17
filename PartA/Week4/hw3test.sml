(* Homework3 Simple Test*)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

use "PartA/Week4/hw3.sml";

val test1 = only_capitals ["A","B","C"] = ["A","B","C"]

val test2 = longest_string1 ["A","bc","C"] = "bc"

val test3 = longest_string2 ["A","bc","C"] = "bc"

val test4a = longest_string3 ["A","bc","C"] = "bc"

val test4b = longest_string4 ["A","B","C"] = "C"

val test2' = longest_string1 [] = ""

val test3' = longest_string2 [] = ""

val test4a' = longest_string3 [] = ""

val test4b' = longest_string4 [] = ""

val test5 = longest_capitalized ["A","bc","C"] = "A"

val test6 = rev_string "abc" = "cba"

val test7 = first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3,4,5] = 4

val test8 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [2,3,4,5,6,7] = NONE

val test9a = count_wildcards Wildcard = 1

val test9b = count_wild_and_variable_lengths (Variable("a")) = 1

val test9c = count_some_var ("x", Variable("x")) = 1

val test10 = check_pat (Variable("x")) = true

val test11 = match (Const(1), UnitP) = NONE

val test12 = first_match Unit [UnitP] = SOME []

val test_challenge_problem1 =
    typecheck_patterns(
        [],
        [ConstP 10, Variable "a"]
    ) = SOME IntT

val test_challenge_problem2 =
    typecheck_patterns(
        [],
        [ConstP 10, Variable "a", ConstructorP ("SOME", Variable "x")]
    ) = NONE

val test_challenge_problem3 =
    typecheck_patterns(
        [],
        [TupleP [Variable "a", ConstP 10, Wildcard], TupleP [Variable "b", Wildcard, ConstP 11], Wildcard]
    ) = SOME (TupleT [Anything, IntT, IntT])

val test_challenge_problem4 =
    typecheck_patterns(
        [("Red","color",UnitT), ("Green","color",UnitT), ("Blue","color",UnitT)],
        [ConstructorP("Red", UnitP), Wildcard]
    ) = SOME (Datatype "color")

val test_challenge_problem5 =
    typecheck_patterns(
        [("Sedan","auto", Datatype "color"), ("Truck","auto",TupleT[IntT, Datatype "color"]), ("SUV","auto",UnitT)],
        [ConstructorP ("Sedan", Variable "a"), ConstructorP ("Truck", TupleP[Variable "b", Wildcard]), Wildcard]
    ) = SOME (Datatype "auto")

val test_challenge_problem6 =
    typecheck_patterns(
        [("Empty","list",UnitT),("List","list",TupleT[Anything, Datatype "list"])],
        [ConstructorP("Empty",UnitP),ConstructorP("List",TupleP[ConstP 10, ConstructorP("Empty",UnitP)]), Wildcard]
    ) = SOME (Datatype "list")

val test_challenge_problem7 =
    typecheck_patterns(
        [("Empty","list",UnitT),("List","list",TupleT[Anything, Datatype "list"])],
        [ConstructorP("Empty",UnitP),ConstructorP("List",TupleP[Variable "k", Wildcard])]
    ) = SOME (Datatype "list")

val test_challenge_problem8 =
    typecheck_patterns(
        [("Sedan","auto", Datatype "color"), ("Truck","auto",TupleT[IntT, Datatype "color"]), ("SUV","auto",UnitT), ("Empty","list",UnitT),("List","list",TupleT[Anything, Datatype "list"])],
        [ConstructorP("Empty",UnitP),ConstructorP("List",TupleP[ConstructorP("Sedan", Variable "c"), Wildcard])]
    ) = SOME (Datatype "list")

val test_challenge_problem9 =
    typecheck_patterns(
        [],
        [TupleP [Variable "x", Variable "y"], TupleP [Wildcard, Wildcard]]
    ) = SOME (TupleT[Anything, Anything])

val test_challenge_problem10 =
    typecheck_patterns(
        [],
        [TupleP [Wildcard, Wildcard], TupleP [Wildcard, TupleP [Wildcard, Wildcard]]]
    ) = SOME (TupleT [Anything, TupleT [Anything, Anything]])

val test_challenge_problem_extra =
    typecheck_patterns(
        [],
        [TupleP [Wildcard], TupleP [Wildcard, Wildcard]]
    ) = NONE