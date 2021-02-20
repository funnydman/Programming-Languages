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

(* 1. *)
fun only_capitals(xs) =
  List.filter(fn x => Char.isUpper(String.sub(x, 0))) xs

(* 2. *)
fun longest_string1 xs =
  List.foldl(fn (x, init) => if String.size(x) > String.size(init) then x else init) "" xs

(* 3. *)
fun longest_string2 xs =
  List.foldr(fn (x, init) => if String.size(x) > String.size(init) then x else init) "" xs

(* 4. *)
fun longest_string_helper f strings =
	List.foldl (fn (str, acc) => if f(String.size str, String.size acc) then str else acc)  "" strings

val longest_string3 = longest_string_helper (fn (len1, len2) => len1 > len2)

val longest_string4 = longest_string_helper (fn (len1, len2) => len1 >= len2)

(* 5. *)
val h  = Char.isUpper o String.sub

fun helper(x, y) =
  if h(x, 0)  andalso h(y, 0)  then
    if String.size(x) > String.size(y) then x else y
  else if h(x, 0) then x else y

fun longest_capitalized(xs) =
  List.foldl(fn (x, init) => helper(x, init)) " " xs

(* 6. *)
fun rev_string(x) =
  let
    val l = String.explode(x)
  in
    (String.implode o List.rev) l
  end

(* 7. *)
fun first_answer f xs =
    case xs of
         [] => raise NoAnswer |
         x::xs' => if isSome(f x) then valOf(f x) else first_answer f xs'

(* 8. *)
fun all_answers f xs =
	let fun helper xs acc =
			case xs of
			   [] => SOME acc |
			   x::xs => case f x of
                NONE => NONE |
                SOME ans => helper xs (acc @ ans)
	in helper xs [] end


(* 9. *)
fun count_wildcards p =
  g (fn () => 1) (fn _ => 0) p

fun count_wild_and_variable_lengths p =
  g (fn () => 1) (fn (x) => String.size(x)) p

fun count_some_var(v, p) =
  g (fn _ => 0) (fn (x) => if x = v then 1 else 0) p


(* 10. *)
fun check_pat p =
    let fun get_vars p=
          case p of
              Variable s => [s]
            | TupleP ps => List.foldl (fn (p,vs) => get_vars p @ vs) [] ps
            | ConstructorP(_,p) => get_vars p
            | _ => []
        fun unique xs =
          case xs of
              [] => true
            | x::xs' => (not (List.exists (fn y => y=x) xs'))
                        andalso unique xs'
    in
        unique (get_vars p)
    end

(* 11. *)
fun match (v, p) =
	case (v, p) of
		(_, Wildcard)	 		 					 => SOME []
	  | (_, Variable s)   		 					 => SOME [(s, v)]
	  | (Unit, UnitP)        		 				 => SOME []
	  | (Const c1, ConstP c)  	 		 			 => if c = c1 then SOME [] else NONE
	  | (Tuple vs, TupleP ps)	         			 => if length vs = length ps
							  			       			then all_answers match (ListPair.zip (vs, ps))
											  			else NONE
	  | (Constructor (s2, v), ConstructorP (s1, p1)) => if s1 = s2 then match (v, p1) else NONE
	  | _ 					 						 => NONE

(* 12. *)
fun first_match v ps =
	SOME (first_answer (fn p => match (v, p)) ps)
	handle NoAnswer => NONE

