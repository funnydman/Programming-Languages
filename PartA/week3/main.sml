(* Building Compound Types *)

val x = {bar=(1+2), foo=(3+4), baz=(false, 9)};
val my_niece = {name ="Amelia", id=132131}
val id = #id my_niece

(* Tuples as Syntactic Sugar *)
val a_pair = (3+1, 4+2);
val a_record = {second=4+2, first=3+1};
val another_pair = {2=5, 1=6};

val x = {1=true, 3="Hi"};

(* val it = (true,7,"Hi") : bool * int * string *)
val y = {3="Hi", 1=true, 2=3+4};

(* Datatype Bindings *)

datatype mytype = TwoInts of int * int
                | Str of string
                | Pizza


val a = Str "Hi"
val b = Str
val c = Pizza
val d = TwoInts(1+2, 3+4)
val e = a

(* Case Expressions *)

(* mytype -> int *)
fun f x =
  case x of
       Pizza => 3
     | Str s => 8
     | TwoInts(i1, i2) => i1 + i2


val r = f Pizza
val r2 = f (Str "Hi")
val s = f (TwoInts (7, 9))

(* Useful Datatypes *)

datatype suit = Club | Diamond | Heart | Spade
datatype rank = Jack | Queen | King | Ace | Num of int

datatype id  = StudentNum of int
             | Name of string

val person = StudentNum 10

(* expression trees, using self-references *)
datatype exp = Constant of int
             | Negate of exp
             | Add of exp * exp
             | Multiply of exp * exp

val res_exp = Add (Constant (10+9), Negate (Constant 4))

(* Recursion *)

fun eval e =
  case e of
       Constant i => i
     | Negate e2 => ~ (eval e2)
     | Add(e1, e2) => (eval e1) + (eval e2)
     | Multiply(e1, e2) => (eval e1) * (eval e2)

val example_ans = eval res_exp

(* Pattern Matching So Far *)

fun max_constant e =
  case e of
       Constant i => i
     | Negate e2 => max_constant e2
     | Add(e1, e2) => Int.max(max_constant e1, max_constant e2)
     | Multiply(e1, e2) => Int.max(max_constant e1, max_constant e2)

val m_const = max_constant res_exp

(* Creating new types *)

(* type aname = t *)

type card = suit * rank

fun is_Queen_of_Spades(c : card) =
  #1 c  = Spade andalso #2 c = Queen

val c1 : card = (Diamond, Ace)
val c2 : suit * rank = (Diamond, Ace)
val c3 = (Diamond, Ace)


(* Lists and Options are Datatypes *)
fun inc_or_zero intoption =
  case intoption of
       NONE => 0
     | SOME i => i +1


fun sum_list xs =
  case xs of
       [] => 0
     | x::xs' => x + sum_list xs'

fun append (xs, ys) =
  case xs of
       [] => ys
     | x::xs' => x :: append(xs', ys)


(* Each of Pattern Matching / Truth About Functions *)
fun sum_triple triple =
  let val (x, y, z) = triple
  in
    x + y + z
  end

(* the same as above *)
fun sum_triple1 (x, y, z) =
  x + y + z


fun full_name {first=x, middle=y, last=z} =
  x ^ " " ^ y ^ " " ^ z

exception ListLengthMismatch

fun zip list_triple =
  case list_triple of
       ([], [], []) => [] |
       (hd1::tl1, hd2::tl2, hd3::tl3) => (hd1, hd2, hd3)::zip(tl1, tl2, tl3) |
       _ => raise ListLengthMismatch

fun unzip lst =
  case lst of
       []=> ([], [], []) |
       (a,b,c)::tl => let val (l1, l2, l3) = unzip tl
                      in
                        (a::l1, b::l2, c::l3)
                      end

val zipping = zip([1,2,3], [1, 2,3], [1,2,3])
val unzipped = unzip zipping

fun nondecreasing xs = (* int list -> bool *)
   case xs of
      [] => true
    | _::[] => true
    | head::(neck::rest) => head <= neck andalso nondecreasing (neck::rest)

datatype sign = Positive | Negative | Zero

(* fun multsign (x1, x2) = (1* int * int -> sign *1) *)
(*    let fun sgn x = if x = 0 then Zero else if x > 0 then Positive else Negative *)
(*    in *)
(*       case (sgn x, sgn y) of *)
(*           (Positive, Positive) => Positive *)
(*         | (Negative, Negative) => Positive *)
(*         | (Zero, _)         => Zero *)
(*         | (_, Zero)         => Zero *)
(*         | _                 => Negative *)
(*    end *)


fun len xs =
  case xs of
       [] => 0
     | _::xs' -> 1 + len xs'

fun eval2 (Constant i) = i
  | eval2 (Negate e2) = ~ (eval2 e2)
  | eval2 (Add(e1, e2)) = (eval2 e1) + (eval2 e2)
  | eval2 (Multiply(e1, e2)) = (eval2 e1) * (eval2 e2)

val res = eval2 res_exp

exception MyUndesirableCondition

fun mydiv (x, y) =
  if y = 0
  then raise MyUndesirableCondition
  else x div y


(* tail recursion *)

(* fun fact n = *)
(*   if n=0 *)
(*   then 1 *)
(*   else n * fact(n-1) *)

(* val r = fact 3 *)


fun fact n =
  let fun aux(n, acc) =
    if n=0
    then acc
    else aux(n-1, acc*n)
  in
    aux(n, 1)
  end

val x = fact 3

(* Accumulators for Tail Recursion *)
(* reverse a list *)
fun rev xs =
  case xs of
       [] => []
     | x::xs' => (rev xs) @ [x]


fun reverse  xs =
  let fun aux(xs, acc) =
  case xs of
       [] => acc
     | x:: xs' => aux(xs', x::acc)
  in
    aux(xs, [])
  end

type user = string * string
type person = int * string

datatype sometype = Person of person | User of user

fun test(xs) =
  case xs of
     [] => [] |
     x::xs' => (x+1) :: test(xs')

val r = test([10,20,30,40])

fun create_user (x) =
  case x of
       Person(i, s) => Person (i, s) |
       User(s1, s2) => User (s1, s2)

val dima = create_user (Person(22, "Dima"))
