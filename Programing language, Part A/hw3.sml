(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)
(* 1 *)

(* string list -> string list *)
fun only_capitals(strs) = 
    List.filter (fn s => Char.isUpper(String.sub(s, 0))) strs

(* 2 *)
(* string list -> string *)
fun longest_string1(strs) =
    foldl (fn(x, y) => if String.size x > String.size y then x else y) "" strs

(* 3 *)
(* string list -> string *)
fun longest_string2(strs) =
    foldl (fn(x, y) => if String.size x >= String.size y then x else y) "" strs

(* 4 *)
fun longest_string_helper compare strs =
	foldl (fn(x, y) => if compare(String.size x, String.size y) then x else y) "" strs

val longest_string3 = longest_string_helper (fn (x, y) => x > y)
val longest_string4 = longest_string_helper (fn (x, y) => x >= y)

(* 5 *)
(* string list -> string *)
val longest_capitalized = longest_string1 o only_capitals


(* 6 *)
(* string -> string *)
val rev_string = String.implode o rev o String.explode

(* 7 *)
(* ’a -> ’b option) -> ’a list -> ’b *)
fun first_answer condition_function some_list = 
	case some_list of
		[] => raise NoAnswer
	  | x::xs' => case condition_function x of
	  			      SOME y => y
	  			    | NONE => first_answer condition_function xs'

(* 8 *)
(* ’a -> ’b option) -> ’a list -> ’b *)
fun all_answers condition_function some_list =
    let
        fun moving_helper(some_list, acc) = 
        	case some_list of
        		[] => SOME acc
        	  | x::xs' => case (condition_function x) of
        	  			      NONE => NONE
        	  			    | SOME y => moving_helper(xs', (y @ acc))
    in
    	moving_helper(some_list, [])
    end

(* 9 a *)
fun count_wildcards p = 
	g (fn () => 1) (fn str => 0) p

(* 9 b *)
fun count_wild_and_variable_lengths p =
    g (fn () => 1) String.size p

(* 9 c *)
fun count_some_var(str, p) =
	g (fn () => 0) (fn x => if x=str then 1 else 0) p

(* 10 *)
fun check_pat p =
    let
    fun get_pattern_list ptn = 
    	case ptn of
    		Variable x => [x]
    	  | TupleP ps => List.foldl (fn(y, acc) => (get_pattern_list y) @ acc) [] ps
    	  | ConstructorP (_,p) => get_pattern_list p
    	  | _ => []

    fun matching_chars str_list =
    	case str_list of
    		[] => false
    	  |	x::xs' => if (List.exists (fn y => x = y) xs')
    	              then true
    	              else matching_chars xs'
    val f = not o matching_chars o get_pattern_list

    in
        f p
    end

(* 11 *)
fun match(v, p) = 
	case (v,p) of
		(_,Wildcard) => SOME []
	  | (sv, Variable s) => SOME [(s, sv)]
	  | (Unit, UnitP) => SOME []
	  | (Const i, ConstP ip) => if (i = 17 andalso ip = 17) then SOME [] else NONE
	  | (Tuple vs, TupleP ps) => if (List.length vs = List.length ps)
	                             then all_answers match (ListPair.zip(vs, ps))
	                             else NONE
	  | (Constructor (cv, v),ConstructorP (cp, p)) => if cv = cp
	  												  then match(v, p)
	  												  else NONE
	  | (_,_) => NONE

(* 12 *)
fun first_match v ps =
  SOME (first_answer (fn x => match(v, x)) ps) handle NoAnswer => NONE
	

