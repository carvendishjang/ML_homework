(* Dan Grossman, CSE341, HW3 Provided Code *)

exception NoAnswer

datatype pattern = WildcardP
		 | VariableP of string
		 | UnitP
		 | ConstantP of int
		 | ConstructorP of string * pattern
		 | TupleP of pattern list

datatype valu = Constant of int
	      | Unit
	      | Constructor of string * valu
	      | Tuple of valu list

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    WildcardP         => f1 ()
	  | VariableP x       => f2 x
	  | ConstructorP(_,p) => r p
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = AnythingT
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | DatatypeT of string

(**** you can put all your code here ****)

				
fun only_lowercase xs = let fun curry f x y = f (y, x) in List.filter (Char.isLower o (curry String.sub 0)) xs end

val m = only_lowercase ["abs", "Cab", "mas", "Tas", "ann", "Leo"]

fun longest_string1 xs = foldl (fn (x, y) => if String.size x > String.size y then x else y) "" xs
			       
val v1 = longest_string1 ["abs", "Cabs", "masss", "Tasssss1", "ann", "Leo", "Tasssss2"]
			
val v2 = longest_string1 ["ab1", "ab2"]

fun longest_string2 xs = foldl (fn (x, y) => if String.size x < String.size y then y else x) "" xs

val n1 = longest_string2 ["abs", "Cabs", "masss", "Tasssss1", "ann", "Leo", "Tasssss2"]
	
val n2 = longest_string2 ["ab1", "ab2"]

fun longest_string_helper f xs = foldl (fn (x, y) => if f(x, y) then x else y) "" xs

val longest_string3 = longest_string_helper (fn (x, y) => String.size x > String.size y)

val p1 = longest_string3 ["abs", "Cabs", "masss", "Tasssss1", "ann", "Leo", "Tasssss2"]

val longest_string4 = longest_string_helper (fn (x, y) => String.size x >= String.size y)

val q1 = longest_string4 ["abs", "Cabs", "masss", "Tasssss1", "ann", "Leo", "Tasssss2"]

val longest_lowercase = longest_string1 o only_lowercase

val q2 = longest_lowercase ["abs", "Cabs", "masss", "Tasssss1", "ann", "Leo", "Tasssss2"]
				 
fun rev_string s = (String.map Char.toUpper o String.rev) s

fun first_answer f xs = case xs of
			    [] => raise NoAnswer 
			  | x :: xs' => case f x of
					    SOME v => v 
					  | NONE => first_answer f xs'
				
fun all_answer f xs = case xs of
			  [] => SOME [] | x :: xs' => let fun aux f acc xs = case xs of
								[] => acc | x :: xs' => case f x of
										 SOME vs => aux f (vs @ acc) xs' | NONE => aux f acc xs'
				     in
					 case aux f [] xs of
					     [] => NONE | v :: vs' => SOME (v :: vs')
				     end

fun g f1 f2 p =
  let 
      val r = g f1 f2 
  in
      case p of
	  WildcardP         => f1 ()
	| VariableP x       => f2 x
	| ConstructorP(_,p) => r p
	| TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	| _                 => 0
  end
      
fun count_wildcards p = g (fn () => 1) (fn x => 0) p

fun count_wild_and_variable_lengths p = g (fn () => 1) String.size p
					  
fun count_a_var (s, p) = g (fn () => 0) (fn x => if x = s then 1 else 0) p

fun check_pat p =
  let fun no_repeats xs =
	case xs of
	    [] => false
	  | x :: xs' => List.exists (fn y => x = y) xs' andalso no_repeats xs'
  in
      let fun string_list p =
	    case p of
		WildcardP         => []
	      | VariableP x       => x :: []
	      | ConstructorP(_,p) => string_list p
	      | TupleP ps         => List.foldl (fn (x,ys) => (string_list x) @ ys) [] ps
	      | _                 => []	
      in
	  (no_repeats o string_list) p
      end
  end
      
fun match (v, p) =
  case (v, p) of
      (_, WildcardP)                          => SOME []
    | (v, VariableP s)                        => SOME ((s, v) :: [])
    | (Unit, UnitP)                           => SOME []
    | (Constant a, ConstantP b)               => if a = b then SOME [] else NONE
    | (Constructor(s1,v), ConstructorP(s2,p)) => if s1 = s2 then match (v, p) else NONE
    | (Tuple vs, TupleP ps)                   => all_answer match (ListPair.zip (vs, ps))
    | _                                       => NONE


			  

val m3 = longest_string4 ["a", "ab", "c", "abc", "abcd", "d", "aaaaaaaaaaaaaaaa"]

fun first_match v ps =  let fun list_add x ys = case ys of [] => [] | y :: ys' => (x, y) :: (list_add x ys')
			in SOME(first_answer match (list_add v ps)) end
			handle NoAnswer => NONE

