(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
(* 1-a *)
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

(* 1-b *)
fun get_substitutions1(strs_lists, str) =
	case strs_lists of
		[] => []
      | x::xs' =>  case all_except_option(str, x) of
				   NONE => get_substitutions1(xs', str)
				 | SOME y => y @ get_substitutions1(xs', str)

(* 1-c *)
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

(* 1-d *)
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

fun same_card(c1 : card, c2 : card) = 
	c1 = c2

(* 2-a *)
fun card_color(s, r) =
	case s of 
		Diamonds => "Red"
	  | Hearts => "Red"
	  | _ => "Black"

(* 2-b *)
fun card_value(s, r) = 
	case r of 
		Num i => i
	  | Ace => 11
	  | _ => 10

(* 2-c *)
fun remove_card(cs, c, e) = 
	case cs of 
		[] => raise e
	  |  x::xs' => if same_card(x, c)
	  			   then xs'
	  			   else x :: remove_card(xs', c, e)

(* 2-d *)
fun all_same_color(cs) = 
	case cs of 
	    [] => true
	  | x::[] => true
	  | x::y::rest => card_color(x) = card_color(y) andalso all_same_color(y::rest)

(* 2-e *)
fun sum_cards(cs) = 
	let 
		fun helper_sum(cs, acc) = 
			case cs of 
				[] => acc
			  | x::xs' => helper_sum(xs', acc + card_value(x))
	in
		helper_sum(cs, 0)
	end

(* 2-f *)
fun score(cs, goal) =
	let
		val tot_val = sum_cards(cs)
		val same_color = all_same_color(cs)
		fun over_above() = 
			if (tot_val > goal)
			then 3 * (tot_val - goal)
			else (goal - tot_val)
		val score_before_color = over_above()
	in
		if same_color
		then score_before_color div 2
		else score_before_color


	end

(* 2-g *)
fun officiate(cards, moves, goal) =
  let
      fun aux(card_list, move_list, hold_list) =
        case move_list of
            [] => score(hold_list, goal)
          | (Discard c)::tl
            => aux(card_list, tl, remove_card(card_list, c, IllegalMove))
          | Draw::tl => case card_list of
                            [] => score(hold_list, goal)
                          | x::xs' => if sum_cards(x::hold_list) > goal
                                      then score(x::hold_list, goal)
                                      else aux(xs', tl, x::hold_list)
  in
      aux(cards, moves, [])
  end


	