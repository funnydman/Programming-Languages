datatype types = TwoInsts of int * int
               | Str of string * string

fun eval e1,  =
  case e of
       TwoInsts(e1,e2) #1 e + #2 e

val exp1 = TwoInsts(10, 20)

val res = eval exp1;
