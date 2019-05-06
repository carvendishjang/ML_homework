(* Dan Grossman, CSE341, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string (s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

								      
fun remove_option (y, xs) =
  case xs of
      [] => NONE
    | x :: xs' => if same_string(y, x) then SOME xs'
		  else case remove_option (y, xs') of
			   NONE => NONE
			 | SOME ys => SOME (x::ys)
								     
  	       
	       


fun all_substitutions1 (xss, y) =
  case xss of [] => []
	    | xs :: xss' => case remove_option(y, xs) of
				NONE => all_substitutions1 (xss', y)
			      | SOME ss => ss @ all_substitutions1 (xss', y)
											
			
fun all_substitutions2 (xss, y) =
  let fun aux (xss, y, acc) =
	case xss of
	    [] => acc
	  | xs :: xss' => aux (xss', y,  case remove_option(y, xs) of
					     NONE => acc | SOME xs => xs @ acc)
  in
      aux (xss, y, [])
  end


fun similar_names (xss, {first=a, middle=b, last=c}) =
  let val ys = a::all_substitutions2 (xss, a)
  in
      let fun aux (ys,{first=a, middle=b, last=c}) =
	    case ys of
		[] => []
	      | y :: ys' => {first=y, last=c, middle=b}::aux (ys', {first=a, middle=b, last=c})
      in
	  aux (ys,{first=a, middle=b, last=c})
      end
  end
      

	     
(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = rank * suit

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)

fun card_color c =
  case c of
      (_, Spades) => Black
    | (_, Clubs) => Black
    | _ => Red

fun card_value c =
  case c of
      (Num i, _) => i
    | (Ace, _) => 11
    | _ => 10
	       
(*fun remove_card (cs, c, e) =
  let fun is_in (cs, c) =
	case cs of
	    [] => false
	  | x::cs' => x=c orelse is_in (cs', c)
  in
      if is_in (cs, c) then *)

fun remove_card (cs : card list, c : card, e) =
  case cs of
      [] => raise e
    | x::cs' => if x=c then cs' else x::remove_card(cs', c, e)
						   
fun all_same_color (cs : card list) =
  case cs of
      [] => true
    | x :: [] => true
    | x :: (y :: cs') => x=y andalso all_same_color (y :: cs')
						    
fun sum_cards cs =
  let fun aux (cs, acc) =
	case cs of
	    [] => acc
	  | c :: cs' => aux (cs', acc + card_value c)
  in
      aux (cs, 0)
  end
      
fun score (cs, goal) =
  let fun pre_score (cs, goal) =
	let val sum = sum_cards cs
	in
	    if sum > goal then 5*(sum-goal) else (goal-sum)
	end
  in
      case all_same_color cs of
	  false => pre_score (cs, goal)
	| _ => pre_score (cs, goal) div 2
  end

      
fun officiate (cs, ms, goal) =
  let fun aux (cs, ms, goal, hs) =
	case (cs, ms) of
	    (_, []) => score(hs, goal)
	  | (_, (Discard c) :: ms') => aux (cs, ms', goal, remove_card(hs, c, IllegalMove))
	  | ([], _) => score(hs, goal)
	  | (d :: cs', Draw :: ms') => if sum_cards (d :: hs) > goal then score(d :: hs, goal) else aux (cs', ms', goal, d :: hs)
  in
      aux (cs, ms, goal, [])
  end
