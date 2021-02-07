(* 14:10 *)

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

fun only_capitals(xs) =
  List.filter(fn x => Char.isUpper(String.sub(x, 0))) xs

(*Foldl-  val it = fn : ('a * 'b -> 'b) -> 'b -> 'a list -> 'b *)
fun longest_string1 xs =
  List.foldl(fn (x, init) => if String.size(x) > String.size(init) then x else init) "" xs

fun longest_string2 xs =
  List.foldr(fn (x, init) => if String.size(x) > String.size(init) then x else init) "" xs


(* 4. *)
fun comp (x: int, y: int) =
  x > y

(* fun longest_string_helper comp (xs, x) = *)
(*   if comp(String.size(x), String.size(init)) then x else init *)


(* val longest_string3 = fn xs => foldl(longest_string_helper(fn (x, init) => *)
(* f(String.size(x), String.size(init)))) "" xs *)

val h  = Char.isUpper o String.sub

fun helper(x, y) =
  if h(x, 0)  andalso h(y, 0)  then
    if String.size(x) > String.size(y) then x else y
  else if h(x, 0) then x else y

(* 5. *)
fun longest_capitalized(xs) =
  List.foldl(fn (x, init) => helper(x, init)) " " xs

(* 6. *)
fun rev_string(x) =
  let
    val l = String.explode(x)
  in
    (String.implode o List.rev) l
  end


val test1 = only_capitals ["A","B","C"] = ["A","B","C"]
val test2 = longest_string1 ["A","bc","C"] = "bc"
val test3 = longest_string2 ["A","bc","C"] = "bc"

(* val test4a = longest_string3 ["A","bc","C"] = "bc" *)

(* val test4b = longest_string4 ["A","B","C"] = "C" *)

val test5 = longest_capitalized ["A","bc","C"] = "A"
val test6 = rev_string "abc" = "cba"

(* f("A", "") -> "A" *)
(* f("bc", "A") -> "A" *)
(* f("C", "A") -> "A" *)
(* val f = helper *)
(* val some = f("C", f("bc", f("A", " "))) *)
