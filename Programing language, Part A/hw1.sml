(* 1 *)
fun is_older(date1 : int*int*int, date2 : int*int*int) = 
    (#1 date1 < #1 date2)
    orelse (#1 date1 = #1 date2 andalso #2 date1 < #2 date2)
    orelse (#1 date1 = #1 date2 andalso #2 date1 = #2 date2 andalso #3 date1 < #3 date2)

(* 2 *)
fun number_in_month(date_list : (int*int*int) list, month : int) = 
    if null date_list
    then 0
    else 
    if #2 (hd date_list) = month
        then 1 + number_in_month(tl date_list, month)
        else 0 + number_in_month(tl date_list, month)

(* 3 *)
fun number_in_months(date_list : (int*int*int) list, month_list : int list) =
	if null month_list
	then 0
	else
	    number_in_month(date_list, hd month_list) + number_in_months(date_list, tl month_list)

(* 4 *)
fun dates_in_month(dates : (int*int*int) list, month : int) = 
    if null dates
    then []
    else
        if #2 (hd dates) = month
        then (hd dates) :: dates_in_month(tl dates, month)
        else dates_in_month(tl dates, month)

(* 5 *)
fun dates_in_months(dates : (int*int*int) list, month : int list) =
    if null month
    then []
    else 
        dates_in_month(dates, hd month) :: dates_in_months(dates, tl month)

(* 6 *)
fun get_nth(string_list : string list, pos : int) = 
    let
        fun start_pos(start, string_list) = 
        if start = pos
        then hd string_list
        else start_pos(start+1, tl string_list)
    in
        start_pos(1, string_list)
    end

(* 7 *)
fun date_to_string(date : int*int*int) = 
    let 
        val month_list = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
    in
        get_nth(month_list, #2 date) ^ " " ^ Int.toString(#3 date)^", "^ Int.toString(#1 date)
    end

(* 8 *)
fun number_before_reaching_sum(sum : int, int_list : int list) = 
    let 
        fun tmp_sum(agg_sum, count, int_list) = 
        if agg_sum >= sum
        then count - 1
        else tmp_sum(agg_sum + (hd int_list), count + 1, tl int_list)
    in
        tmp_sum(0, 0, int_list)
    end

(* 9 *)

    