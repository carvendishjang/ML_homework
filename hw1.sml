

fun is_older (x : int * int * int, y : int * int * int) =
  #3 x > #3 y orelse (#3 x = #3 y andalso #2 x > #2 y) orelse (#3 x = #3 y andalso #2 x = #2 y andalso #1 x > #1 y)

								  
fun number_in_month (xs : (int * int * int) list, y : int) =
  if null xs
  then 0
  else if #2 (hd xs) = y
  then 1 + number_in_month (tl xs, y)
  else number_in_month (tl xs, y)

		       
fun number_in_months (xs : (int * int * int) list, ys : int list) =
  if null ys
  then 0
  else number_in_month (xs, hd ys) + number_in_months (xs, tl ys)

fun dates_in_month (xs : (int * int * int) list, y : int) =
  if null xs
  then []
  else if #2 (hd xs) = y
  then hd xs :: dates_in_month (tl xs, y)
  else dates_in_month (tl xs, y)

		      
fun dates_in_months (xs : (int * int * int) list, ys : int list) =
  if null ys
  then []
  else dates_in_month (xs, hd ys) @ dates_in_months (xs, tl ys)

						    
fun get_nth (xs : string list, n : int) =
  if n = 1
  then hd xs
  else get_nth (tl xs, n-1)

	       
fun date_to_string (x : int * int * int) =
  let
     val ms = ["January", "February", "March", "April", "May", "June",
	    "July", "August", "September", "October", "November", "December"]
  in
      get_nth(ms, #2 x) ^ "-" ^ Int.toString(#1 x) ^ "-" ^ Int.toString(#3 x)
  end

      
fun number_before_reaching_sum (sum : int, xs : int list) =
  if hd xs >= sum
  then 0
  else 1 + number_before_reaching_sum (sum-(hd xs), tl xs)

				      
fun what_month (x : int) =
  let
      val ms = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
  in
      1 + number_before_reaching_sum (x, ms)
  end

      
fun month_range (day1 : int, day2 : int) =
  if day1 > day2
  then []
  else (what_month day1) :: month_range (day1+1, day2)

					
fun oldest (xs : (int * int * int) list) =
  if null xs
  then NONE
  else let fun oldest_nonempty (xs : (int * int * int) list) =
	     if null (tl xs)
	     then hd xs
	     else let val tl_ans = oldest_nonempty(tl xs)
		  in
		      if is_older (hd xs, tl_ans)
		      then hd xs
		      else tl_ans
		  end
       in
	   SOME(oldest_nonempty xs)
       end

	   
fun cumulative_sum (xs : int list) =
  if null xs
  then []	       
  else let fun aux (x : int, xs : int list) =
	     if null xs
	     then []
	     else
		 x + hd xs :: tl xs
       in
	   hd xs :: cumulative_sum (aux (hd xs, tl xs))
       end

	   
fun remove_dup (xs : int list) =
  if null xs
  then []
  else let fun not_in (x : int, xs : int list) =
	     null xs orelse (not (x = hd xs) andalso not_in (x, tl xs))
       in
	   if not_in (hd xs, remove_dup(tl xs))
	   then hd xs :: remove_dup(tl xs)
	   else remove_dup(tl xs)
       end

	   
fun number_in_months_challenge (xs : (int * int * int) list, ys : int list) =
  let
      fun remove_dup (xs : int list) =
	if null xs
	then []
	else let fun not_in (x : int, xs : int list) =
		   null xs orelse (not (x = hd xs) andalso not_in (x, tl xs))
	     in
		 if not_in (hd xs, remove_dup(tl xs))
		 then hd xs :: remove_dup(tl xs)
		 else remove_dup(tl xs)
	     end
  in
      number_in_months (xs, remove_dup ys)
  end
      
fun dates_in_months_challenge (xs : (int * int * int) list, ys : int list) =
  let
      fun remove_dup (xs : int list) =
	if null xs
	then []
	else let fun not_in (x : int, xs : int list) =
		   null xs orelse (not (x = hd xs) andalso not_in (x, tl xs))
	     in
		 if not_in (hd xs, remove_dup(tl xs))
		 then hd xs :: remove_dup(tl xs)
		 else remove_dup(tl xs)
	     end
  in
      dates_in_months (xs, remove_dup ys)
  end
					 
      
fun reasonable_date (x : int * int * int) =
  let
     fun check_year (x : int * int * int) = #3 x > 0 
  in
      check_year x
  end
  andalso
  let
      fun check_month (x : int * int * int) = #2 x >0 andalso #2 x < 13
  in
      check_month x
  end
  andalso
  let
      fun check_day (x : int * int * int) =
	let
	    fun is_in (x : int, xs : int list) =
	      not (null xs) andalso (x = hd xs orelse is_in (x, tl xs))
	in
	    if is_in (#2 x, [1,3,5,7,8,10,12])
	    then #1 x > 0 andalso #1 x < 32
	    else if is_in (#2 x, [4,6,9,11])
	    then #1 x > 0 andalso #1 x < 31
	    else
		let
		    fun leap_year (x : int * int * int) =
		      (#3 x) mod 400 = 0 orelse ((#3 x) mod 4 = 0 andalso not ((#3 x) mod 100 = 0))
		in
		    if leap_year x
		    then #1 x > 0 andalso #1 x < 30
		    else #1 x > 0 andalso #1 x < 29
		end
	end
  in
      check_day x
  end
      
		  
