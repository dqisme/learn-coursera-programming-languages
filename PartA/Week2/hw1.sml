fun is_older (one_date : int * int * int, another_date : int * int * int) =
    (#1 one_date) < (#1 another_date)
    orelse (#1 one_date) = (#1 another_date) andalso (
        (#2 one_date) < (#2 another_date)
        orelse (#2 one_date) = (#2 another_date) andalso (#3 one_date) < (#3 another_date))

fun number_in_month (list_of_dates : (int * int * int) list, month : int) =
    if null list_of_dates then 0
    else if (#2 (hd list_of_dates)) = month then 1 + number_in_month(tl list_of_dates, month)
    else number_in_month(tl list_of_dates, month)

fun number_in_months (list_of_dates : (int * int * int) list, list_of_months : int list) =
    if null list_of_months then 0
    else number_in_month(list_of_dates, hd list_of_months) + number_in_months(list_of_dates, tl list_of_months)

fun dates_in_month (list_of_dates : (int * int * int) list, month : int) =
    if null list_of_dates then []
    else if (#2 (hd list_of_dates)) = month then (hd list_of_dates) :: dates_in_month(tl list_of_dates, month)
    else dates_in_month(tl list_of_dates, month)

fun dates_in_months (list_of_dates : (int * int * int) list, list_of_months: int list) =
    if null list_of_months then []
    else dates_in_month(list_of_dates, hd list_of_months) @ dates_in_months(list_of_dates, tl list_of_months)

fun get_nth (list_of_strings : string list, n : int) =
    if n = 1 then hd list_of_strings
    else get_nth(tl list_of_strings, n - 1)

fun date_to_string (year : int, month : int, day : int) =
    get_nth(["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"], month) ^ " " ^ Int.toString day ^ ", " ^ Int.toString year

fun number_before_reaching_sum (sum : int, list_of_numbers : int list) =
    let val next_sum = sum - hd list_of_numbers
    in
	if next_sum <= 0 then 0
	else 1 + number_before_reaching_sum(next_sum, tl list_of_numbers)
    end

fun what_month (day_of_year : int) =
    number_before_reaching_sum(day_of_year, [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]) + 1

fun month_range (day1 : int, day2 : int) =
    if day1 > day2 then []
    else what_month(day1) :: month_range(day1 + 1, day2)

fun oldest (list_of_dates: (int * int * int) list) =
    if null list_of_dates then NONE
    else let
	val oldest_in_tail = oldest(tl list_of_dates)
    in
	if isSome oldest_in_tail andalso is_older(valOf oldest_in_tail, hd list_of_dates) then oldest_in_tail
	else SOME (hd list_of_dates)
    end

fun omit (list_of_numbers : int list, number_to_omit : int) =
    if null list_of_numbers then []
    else let
	val first_number = hd list_of_numbers
	val omitted_tail = omit(tl list_of_numbers, number_to_omit)
    in
	if first_number = number_to_omit then omitted_tail
	else first_number :: omitted_tail
    end

	     
fun dedupe (list_of_numbers : int list) =
    if null list_of_numbers then []
    else let
	val first_number = hd list_of_numbers
    in
	first_number :: dedupe(omit(tl list_of_numbers, first_number))
    end

fun number_in_months_challenge (list_of_dates : (int * int * int) list, list_of_months : int list) =
    number_in_months(list_of_dates, dedupe(list_of_months))

fun dates_in_months_challenge (list_of_dates : (int * int * int) list, list_of_months: int list) =
    dates_in_months(list_of_dates, dedupe(list_of_months))

fun get_nth_number (list_of_numbers: int list, n : int) =
    if n = 1 then hd list_of_numbers
    else get_nth_number(tl list_of_numbers, n - 1)

fun is_leap_year (year: int) =
    year mod 400 = 0 orelse year mod 4 = 0 andalso year mod 100 <> 0

fun last_day_in_a_month_of_a_year (year: int, month: int) =
    if is_leap_year(year) andalso month = 2 then 29
    else get_nth_number([31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31], month)
		       

fun reasonable_date (year: int, month: int, day: int) =
    year > 0
    andalso month >=1 andalso month <= 12
    andalso day >= 1 andalso day <= last_day_in_a_month_of_a_year(year, month)
