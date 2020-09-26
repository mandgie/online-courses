(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
fun all_except_option(str, strs) =
	let
		fun remove_string(strs) =
			case strs of
				[] => []
			  |  x::xs' => if (same_string(x, str))
			  			   then xs'
			  			   else x :: remove_string(xs')
		val remaining_list = remove_string(strs)
	in
	    if remaining_list = strs
	    then NONE
	    else SOME remaining_list
    end

(* b *)
fun get_substitutions1(strs_lists, str) =
	case strs_lists of
		[] => []
      | x::xs' =>  case all_except_option(str, x) of
				   NONE => get_substitutions1(xs', str)
				 | SOME y => y @ get_substitutions1(xs', str)

(* c *)
fun get_substitutions2(strs_lists, str) =
	let
		fun moving_helper(current, acc) = 
			case current of
				[] => acc
      		  | x::xs' =>  case all_except_option(str, x) of
								NONE => get_substitutions2(xs', str)
				 			  | SOME y => moving_helper(xs', acc @ y)
	in
		moving_helper(strs_lists, [])
	end

(* d *)
fun similar_names(strs, name) = 
	let
		val {first=frt, middle=mid, last=lst} = name
		val final_names = get_substitutions2(strs, frt)
		fun full_name(names) = 
			case names of
				[] => []
			  | x::xs' => {first=x, middle=mid, last=lst} :: full_name(xs')
	in
		name :: full_name(final_names)
	end



(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)

	