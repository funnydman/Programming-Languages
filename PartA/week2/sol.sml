fun is_older((y1 : int , m1: int, d1: int), (y2:int, m2: int, d2: int)) =
  if y2 < y1 then false else
  if m2 < m1 then false else
  if d2 < d1 then false else true


fun number_in_month(dates: (int * int * int) list, month: int) =
  if null dates
  then 0
  else if ((#2 (hd dates)) = month)
  then 1 + number_in_month(tl dates, month)
  else number_in_month(tl dates, month)


fun number_in_months(dates: (int * int * int) list, months: int list) =
  if null months
  then 0
  else number_in_month(dates, hd months) + number_in_months(dates, tl months)


fun dates_in_month(dates: (int * int * int) list, month: int) =
  if null dates
  then []
  else if ((#2 (hd dates)) = month)
  then (hd dates) :: dates_in_month(tl dates, month)
  else dates_in_month(tl dates, month)


fun dates_in_months(dates: (int * int * int) list, months: int list) =
  if null months
  then []
  else dates_in_month(dates, hd months) @ dates_in_months(dates, tl months)

fun get_nth(xs: string list, n: int) =
  if n-1=0
  then hd xs
  else get_nth(tl xs, n-1)

fun date_to_string(date: (int * int * int)) =
  let val months = ["January", "February", "March", "April", "May", "June", "July",
  "August", "September", "October", "November", "December"]
  in
    get_nth(months, #2 date) ^ " " ^ Int.toString(#3 date) ^ ", " ^
    Int.toString(#1 date)
  end

fun number_before_reaching_sum(sum: int, xs: int list) =
  if sum > (hd xs)
  then number_before_reaching_sum(sum - (hd xs), tl xs) + 1
  else 0

fun what_month (day: int) =
  let val months = [31,28,31,30,31,30,31,31,30,31,30,31]
  in
    number_before_reaching_sum(day, months) + 1
  end

fun month_range(day1: int, day2: int) =
  if day1 > day2
  then []
  else what_month(day1) :: month_range(day1+1, day2)

fun oldest(dates: (int*int*int) list) =
  if null dates
  then NONE
  else let
    fun oldest_nonempty(dates: (int * int * int) list) =
      if null (tl dates)
      then hd dates
      else let val tl_ans = oldest_nonempty(tl dates)
           in
             if is_older(hd dates, tl_ans)
             then hd dates
             else tl_ans
          end
  in
    SOME (oldest_nonempty dates)
  end
