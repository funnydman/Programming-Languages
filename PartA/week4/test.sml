val test1 = only_capitals ["A","B","C"] = ["A","B","C"]
val test2 = longest_string1 ["A","bc","C"] = "bc"
val test3 = longest_string2 ["A","bc","C"] = "bc"

val test4a = longest_string3 ["A","bc","C"] = "bc"

val test4b = longest_string4 ["A","B","C"] = "C"

val test5 = longest_capitalized ["A","bc","C"] = "A"
val test6 = rev_string "abc" = "cba"
val test7 = first_answer (fn x => if x > 3 then SOME x else NONE) [1, 2, 3, 4, 5] = 4

val test8b = all_answers (fn x => if x = 1 then SOME [x] else NONE) [2,3,4,5,6,7] = NONE
val test8c = all_answers (fn x => if x = 1 then SOME [x] else NONE) [] = SOME []

val test8d = all_answers (fn x => if x > 2 then SOME [x] else NONE) [1, 2,3,4,5,6,7]

val test_p = (ConstructorP("abc", (TupleP [Wildcard, Variable "xxx", Wildcard])))

val test9a = count_wildcards Wildcard = 1

val test9b = count_wild_and_variable_lengths test_p = 5

val test9c = count_some_var ("x", (TupleP [Variable("x"), Wildcard, Variable("x")])) = 2

val test10 = check_pat (Variable("x")) = true

val test11 = match (Const(1), UnitP) = NONE

val test12 = first_match Unit [UnitP] = SOME []
