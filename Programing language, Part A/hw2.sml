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


(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)

	