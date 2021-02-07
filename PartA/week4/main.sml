(* ('a > 'a) * int * 'a -> 'a *)
fun n_times (f,n,x) =
  if n=0
  then x
  else f (n_times(f, n-1, x))

fun inc x = x+1
fun double x = x + x
val x1 = n_times(inc, 4, 7)


(* fun triple_n_times (n, x) = *)
(*   n_times (let fun triple x = 3*x in triple end, n, x) *)

(* with anonymous funciton *)
fun triple_n_times(n,x) =
  n_times((fn x => 3*x), n, x)

(* function wrapping *)
val rev = List.rev

val res  = rev [1,2,3]

fun map(f,xs) =
  case xs of
       [] => [] |
       x::xs' => (f x)::map(f, xs')


fun filter(f, xs) =
  case xs of
       [] => [] |
       x::xs' => if f x
                 then x::(filter (f, xs'))
                 else filter(f, xs')

fun zip xs ys =
  case (xs, ys) of
       ([], []) => [] |
       (x::xs', y::ys') => (x, y)::(zip xs' ys') |
       _ => raise Empty

fun is_even v =
  (v mod 2 = 0)

val x1 = map((fn x => x+1), [4,8,12,16])
val x2 = map(hd, [[1,2], [3,4]])
val x3 = filter(is_even, [1,2,3,4,5])

fun get_all_even_snd xs = filter((fn (_,v) => v mod 2 = 0), xs)

fun greaterThanX x = fn y => y > x (* int -> (int -> bool) *)

fun noNegative xs = filter(greaterThanX ~1, xs)

fun allGreater (xs, n) = filter(fn x => x > n, xs)


val test1 = greaterThanX(10)(15)

(* fun sayHello = print "Hello, " *)

fun fold (f, acc, xs) =
  case xs of
       [] => acc |
       x::xs => fold(f, f(acc, x), xs)

(* examples not using private data *)
fun f1 xs = fold((fn (x, y) => x+y ), 0, xs)
val res = f1 [1, 2, 3, 4]

fun f2 xs = fold(( fn (x, y) => x andalso y >=0), true, xs)

(* counting the number of elements between lo and hi, inclusive *)
fun f3 (xs, lo, hi) =
  fold ((fn (x, y)=> x + (if y>= lo andalso y<=hi then 1 else 0)), 0, xs)

fun f5 (g, xs) = fold((fn(x, y) => x andalso g y), true, xs)

fun f4again (xs, s) =
  let
    val i  = String.size s
  in
    f5(fn y => String.size y < i, xs)
  end

fun compose (f, g) = fn x => f(g x)

(* function composition *)
val sqrt_of_abs = Math.sqrt o Real.fromInt o abs

infix !>
fun x !> f = f x

(* like pipelines *)
fun sqrt_of_abs1 i = i !> abs !> Real.fromInt !> Math.sqrt

fun backup1 (f, g) = fn x => case f x of
                                  NONE => g x |
                                  SOME y => y

fun backup2 (f, g) = fn x => f x handle _ => g x


(* Currying *)
val sorted3 = fn x => fn y => fn z => z >= y andalso y >= x
val t1 = sorted3 7 9 11
val t2 = ((sorted3 7) 9) 11

(* syntax sugar for version above *)
fun sorted3_nicer x y z = z >= y andalso y >= x
val t1 = sorted3_nicer 7 9 11
val t4 = ((sorted3_nicer 7) 9) 11 (* the same as above *)

(* Currying Wrapup *)
fun curry f x y = f (x,y)

fun uncurry f (x,y) = f x y (* uncurry foo *)

fun other_curry f x y = f y x

(* example *)
fun range (i, j) = if i > j then [] else i::range(i+1, j)

val countup = curry range 1
val xs = countup 7


(* examples *)
fun sum x y = x + y

val curried_sum = fn x => fn y => sum x y

fun curried_sum x y = sum x y

val add10 = curried_sum 10

val res = add10 20

(* refereces *)
val x = ref 42
val y = ref 42
val z = x
val _ = x := 43
(* !y  - retrieve content *)
val w = (!y) + (!z)

(* Show functions in list module *)
(* - structure X = List; *)
(* structure X : LIST_2015? *)
(* - signature X = LIST; *)
