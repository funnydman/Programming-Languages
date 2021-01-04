(* this is a comment! *)
print "Hello world\n";
val x = 10;

val y = 5;

val z = x + y;

val abs_of_z = if z < 0 then 0-z else z;

(* var res = if e1 then e2 else e3;
e1 is bool, e2 and e3 have the same type(t),
so the result always has type t
use "main.sml"; <-- load file in repl *)

val neg = ~ 10;

val res1 = x div y; (* int division *)

val res2 = 10.0 / 5.0; (* for real number, type : real *)

(* Shadowing *)

val x = 99; (* this is not an assigment smt *)

(*
General e0 (e1,...,en)
no way to specify variable number of arguments
Type checking:
if
e0 has some type (t1, * ... * tn) -> t
e1 has type t1, ..., en has type tn
then
  e0 (e1,...,en) has type t

*)
fun pow (x : int, y : int) =
  if y =0
  then 1
  else x * pow(x, y-1)


fun cube(x : int) =
  pow(x, 3)


val eight = cube(2);

(* tuples *)
val x = (10, 20);

fun swap (pr : int*bool) =
  (#2 pr, #1 pr)

(* (int * int) * (int * int) - > int*)
fun sum_two_pairs (pr1 : int * int, pr2 : int * int) =
  (#1 pr1) + (#2 pr1) + (#1 pr2) + (#2 pr2)

fun div_mod (x : int, y : int) =
  (x div y, x mod y)

fun sort_pair(pr : int * int) =
  if (#1 pr) < (#2 pr)
  then pr
  else (#2 pr, #1 pr)


val x1 = (7, (true,9)); (* int * (bool * int) *)
val x2 = #1 (#2 x1); (* bool *)
val x3 = (#2 x1);

(* List:
Despite nested tuples, the type of a variable still "commits" to a particular
"amount" of data
In contrast, a list:
- Can have any number of elements
- But all list elements have the same type
*)

val alist = [10, 20, 30, 40];
val calculated = [(1+2), 3+4, 7]; (* [3,7, 7] *)
val bools = [true, false, true];


(* "Cons" *)
val x = [1, 2, 3];
0::x;
(* [0, 1, 2, 3 ] *)

(*
Accessing Lists:

null e evaluates to true if and  only if e evaluates to []
hd e - get 0 index element
tl - get list with not head element, result is a list, (raises exception if e
evaluates to [])

empty list has type alpha list, means can has any type;
*)

(*
List Functions:
*)

fun sum_list(alist : int list) =
  if null alist
  then 0
  else hd alist +  sum_list(tl alist);

fun countdown (x : int) =
  if x=0
  then []
  else x :: countdown(x-1)

fun append(xs: int list, ys : int list) =
  if null xs
  then ys
  else (hd xs) :: append((tl xs), ys)

(* functions over pairs of lists *)

fun sum_pair_list (xs: (int * int) list) =
  if null xs
  then 0
  else #1 (hd xs) + #2 (hd xs) + sum_pair_list(tl xs)

val res = sum_pair_list([(3, 4), (5, 6)]);

fun first (xs: (int * int ) list ) =
  if null xs
  then []
  else (#1 (hd xs)) :: first(tl xs)


fun second (xs: (int * int ) list ) =
  if null xs
  then []
  else (#1 (hd xs)) :: second(tl xs)

fun sum_pair_list2 (xs: (int * int) list ) =
  (sum_list (first xs)) + (sum_list (second xs))

(*
let expressions  - local variables
let expression specifies local scope
*)

fun silly(z : int) =
  let
    val x = if z > 0 then z else 34
  in
    if x > y then x * 2 else y * y
  end


fun silly () =
  let
    val x = 1
  in
    (let val x = 2 in x + 1 end) + (let val y = x+2 in y + 1 end)
  end

(* Nested functions *)
fun countup_from(x : int) =
  let
    fun count (from : int, to : int) =
      if from=to
      then to::[]
      else from :: count(from+1, to)
  in
    count(1,x)
  end

fun find_max (xs : int list) =
  if null xs
  then 0
  else if null (tl xs)
  then hd xs
  else
    let val tl_ans = find_max(tl xs)
    in
      if hd xs > tl_ans
      then hd xs
      else tl_ans
    end


(* options *)
(* Better implementation *)
fun max1(xs: int list) =
  if null xs
  then NONE
  else
    let val tl_ans = max1(tl xs)
    in if isSome tl_ans andalso valOf tl_ans > hd xs
       then tl_ans
       else SOME (hd xs)
    end

val res = ((valOf (max1 [3, 7, 5]))) + 1

fun max2 (xs: int list) =
  if null xs
  then NONE
  else let
    fun max_nonempty (xs: int list) =
      if null (tl xs)
      then hd xs
      else let val tl_ans = max_nonempty(tl xs)
           in
             if hd xs > tl_ans
             then hd xs
             else tl_ans
           end
      in
        SOME (max_nonempty xs)
      end

(*
Boolean operations:
1) e1 andalso e2
2) e1 orelse e2
3) not e1

Languages does not need andalso, orelse, not
ex:
if e1
then e2
else false

is equivalent to (e1 andalso e2)
*)

