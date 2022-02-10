(* HOMEWORK 2 *)



(* Boilerplate Stuff *)

type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal;;

let rec filter f a =
  match a with
  | [] -> []
  | hd :: tl -> if f hd
    then hd :: filter f tl
    else filter f tl;;

type awksub_nonterminals =
  | Expr | Term | Lvalue | Incrop | Binop | Num | Bad1 | Bad2

let awkish_grammar =
  (Expr,
   function
     | Expr ->
         [[N Term; N Binop; N Expr];
          [N Term]]
     | Term ->
	 [[N Num];
    [N Bad1];
	  [N Lvalue];
	  [N Incrop; N Lvalue];
	  [N Lvalue; N Incrop];
	  [T"("; N Expr; T")"]]
     | Lvalue ->
	 [[T"$"; N Expr]]
     | Incrop ->
	 [[T"++"];
	  [T"--"]]
     | Binop ->
	 [[T"+"];
	  [T"-"]]
     | Num ->
	 [[T"0"]; [T"1"]; [T"2"]; [T"3"]; [T"4"];
	  [T"5"]; [T"6"]; [T"7"]; [T"8"]; [T"9"]]
    | Bad1 ->
    [[N Bad2]]
    | Bad2 ->
    [[N Bad1]])

 (* Things to remove blind alleys from new grammars. *)
 let rec is_member x list =
 match list with
 | [] -> false
 | hd :: tl ->
   if hd = x then true
   else is_member x tl;;

let rec subset a b = 
 match a with
 | [] -> true
 | hd :: tl ->
   if is_member hd b then subset tl b
   else false;;

let equal_sets a b =
 (subset a b) && (subset b a);;

 let rec remove_duplicates list =
  match list with
  | [] -> []
  | hd :: tl ->
    if is_member hd tl then remove_duplicates tl
    else hd :: remove_duplicates tl;;

let rec set_union a b = 
  match a with
  | [] -> (remove_duplicates b)
  | hd :: tl ->
    if is_member hd b then set_union tl (remove_duplicates b)
    else set_union tl (hd::(remove_duplicates b));;

let rec set_all_union a =
  match a with
  | [] -> []
  | [last] -> last
  | hd :: tl ->
    set_all_union (((set_union hd (List.hd tl))) :: (List.tl tl));;

let rec computed_fixed_point eq f x = 
  if eq x (f x) then x
  else computed_fixed_point eq f (f x);;

(* Tells if a character is either a terminal or is a nonterminal marked as terminatable. *)
let rec char_terms c dict =
  match c with
  | T t -> true
  | N n -> 
    match dict with
    | [] -> false
    | (term, status) :: rest -> if term = c then status else char_terms c rest;;

(* Tells if a word contains only terminals and nonterminals marked as terminatable. *)
let rec word_terms word dict =
  match word with
  | [] -> true
  | hd :: tl -> (char_terms hd dict) && (word_terms tl dict);;

(* Concat something onto the start of every list in a set: set_concat 0 [[1];[2];[3]] -> [[0;1];[0;2];[0;3]] *)
let rec set_concat a b = 
  match b with
  | [] -> []
  | hd :: tl -> (a :: hd) :: (set_concat a tl);;

(* Returns a list where a function is applied to every element of a given list. *)
let rec apply_to_all f a =
  match a with
  | [] -> []
  | hd :: tl -> (f hd) :: (apply_to_all f tl);;

(* Given a list of items and a function that takes in two vals from the list and outputs another item of the list type, collapse to the end. Useful for && and || on boolean arrays. *)
let rec collapse_list f a =
  match a with 
  | hd :: [] -> hd
  | hd :: tl -> f hd (collapse_list f tl);;

(* Tells if a nonterminal character now terminates when passed into the production function. *)
let nonterm_terms nonterm dict prod =
  if char_terms nonterm dict then true else
  match nonterm with
  | N n -> let alternatives = prod n in
    let booleanize = apply_to_all (fun word -> word_terms word dict) alternatives in
     collapse_list (||) booleanize;;

(* Given a grammar g, return all nonterminal characters reachable from the start of the grammar *)
let get_nonterms g =
  let iterate_nonterms nonterms prod =
    let all_alternatives = apply_to_all (fun c -> match c with | N n -> prod n | T t -> []) nonterms in
    let new_nonterms = set_all_union (apply_to_all set_all_union all_alternatives) in
    filter (fun x -> match x with | N n -> true | T t -> false) new_nonterms 
  in match g with 
  | trigger, prod -> computed_fixed_point equal_sets (fun nonterms -> iterate_nonterms nonterms prod) [N trigger];;

(* Given a completed blind-alley search dictionary, this removes any alternatives containing a blind alley. *)
let clean_prod prod dict =
  fun term -> 
    let alternatives = prod term in
    filter (fun word -> word_terms word dict) alternatives;;

(* Given a grammar g, returns a grammar such that all blind alleys are removed from g's production function. *)
let filter_blind_alleys g =
  match g with
  | trigger, prod ->
    let dictionary = apply_to_all (fun f -> (f, false)) (get_nonterms g) in
    let rec iterate_dict dict dict2 = 
      match dict with
      | [] -> []
      | (v, status) :: rest ->  (v, nonterm_terms v dict2 prod) :: (iterate_dict rest dict2)
    in let finished_dict = computed_fixed_point equal_sets (fun x -> iterate_dict x x) dictionary
    in (trigger, clean_prod prod finished_dict);;


(* ---------- *)
(* Question 1 *)
(* ---------- *)

let rec production_function symbol g =
  match g with
  | start, rules ->
    match rules with
    | [] -> []
    | rule :: rest ->
      match rule with
      | trigger, word ->
        if symbol = trigger 
          then word :: production_function symbol (trigger, rest)
          else production_function symbol (trigger, rest);;

(* This directly converts the grammar, and does nothing about blind alleys. *)
let convert_grammar g =
  match g with
  | start, rules -> start, (fun symbol -> production_function symbol g);;

let alternative_list symbol g =
  match g with
  | start, prod -> prod symbol;;

let get_trigger g = 
  match g with
  | trigger, prod -> trigger;;

let get_prod g =
  match g with
  | trigger, prod -> prod;;

(* ---------- *)
(* Question 2 *)
(* ---------- *)

type ('nonterminal, 'terminal) parse_tree =
  | Node of 'nonterminal * ('nonterminal, 'terminal) parse_tree list
  | Leaf of 'terminal

(* Quick list reverser to make traverse_tree work left to right properly without performance overhead. *)
let reverse_list input =
  let rec reverse_internal inp out =  
    match inp with
    | [] -> out
    | hd :: tl -> reverse_internal tl (hd :: out)
  in reverse_internal input [];;

(* Takes in a tree and a log of characters visited so far - with how it works, it has to output results in reverse order. *)
let rec traverse_tree tree log =
  match tree with
  | Leaf term -> term :: log
  | Node (term, branches) ->
    match branches with
    | [] -> log
    | branch :: rest -> traverse_tree (Node (term, rest)) (traverse_tree branch log);;

(* Just a wrapper for the actual algorithms taking place above. *)
let parse_tree_leaves tree =
  reverse_list (traverse_tree tree []);;


(* ---------- *)
(* Question 3 *)
(* ---------- *)

let accept_none x = None;;

let accept_all x = Some x;;

let accept_empty = function
  | [] -> Some []
  | _ -> None;;

(* Just checks if the word is only consisting of terminal characters *)
let rec word_terminates word =
  match word with
  | [] -> true
  | hd :: tl -> match hd with
    | T t -> word_terminates tl
    | N n -> false;;


(* Arbitrary list concaternation, for joining alternatives to the rest of words. *)
let word_append a b =
  let rec append_internal c d =
    match c with 
    | [] -> d
    | hd :: tl -> append_internal tl (hd :: d)
  in append_internal (reverse_list a) b;;



(* This will return an array of words, where every possible rule in order is applied to the first non-terminal character in the grammar. *)
let rec get_children word prod =
  match word with
  | [] -> []
  | T t :: rest -> set_concat (T t) (get_children rest prod)
  | N n :: rest -> let alternatives = prod n in (apply_to_all (fun alt -> word_append alt rest) alternatives)

(* This is how to check if we need to backtrack; compare terminal characters up until either the end of both strings or a non-terminal is encountered *)
let rec terminals_match a b =
  match a with 
  | [] -> true
  | (N n1) :: rest1 -> true
  | (T t1) :: rest1 ->
    match b with
    | [] -> false
    | (N n2) :: rest2 -> true
    | (T t2) :: rest2 -> if t1 = t2 then terminals_match rest1 rest2 else false;;

(* Returns alternative list for first nonterminal in word *)
let rec get_rules word prod = 
  match word with
  | [] -> []
  | (T t) :: rest -> get_rules rest prod
  | (N n) :: rest -> prod n;;

(* Returns the part of b that extends beyond a *)
let rec get_suffix a b =
  match a with
  | [] -> b
  | ahd :: atl ->
    match b with
    | [] -> []
    | bhd :: btl -> get_suffix atl btl;;

(* Takes in an array of terminal characters and just returns the values of the characters *)
let rec determinalize a =
  match a with
  | [] -> []
  | (T t) :: rest -> t :: (determinalize rest)
  | (N n) :: rest -> determinalize rest;;

(* Do the opposite of above *)
let rec terminatize a =
  match a with
  | [] -> []
  | hd :: tl -> (T hd) :: terminatize tl;;

(* The meat and bones of the matching algorithm - word (what you currently have), matcher (what you're trying to get to), prod (production function), accept (acceptor for early search end)*)
let rec search_for_match word matcher prod accept = 
  let children = get_children word prod in
  let filtered_children = filter (fun child -> terminals_match child matcher) children in
  let rec search_until_found a = 
    match a with
    | [] -> None
    | child :: rest -> let child_search = (if word_terminates child then accept (determinalize (get_suffix child matcher)) else search_for_match child matcher prod accept) in
      match child_search with
      | Some s -> Some s
      | None -> search_until_found rest
  in search_until_found filtered_children;;

(* Return a function that will call a properly formatted search_for_match *)
let make_matcher bad_g = 
  let g = filter_blind_alleys bad_g in
  (fun accept frag ->
    match g with
    | trigger, prod ->
      search_for_match [N trigger] (terminatize frag) prod accept);;





(* ---------- *)
(* Question 4 *)
(* ---------- *)

(* For a = [1;2;3], b = [4;5;6], splice_arrays a b = [(1,4);(2,5);(3;6)]. Idk makes pruning some things easier. *)
let rec splice_arrays a b =
  match a with 
  | [] -> []
  | ah :: at -> (ah, List.hd b) :: splice_arrays at (List.tl b);;

(* Modified from the search_for_match to only look for perfect matches, and record the list of rules used to get to that point. *)
let rec get_path word matcher prod = 
  let children = get_children word prod in
  let rules = get_rules word prod in
  let c_and_r = splice_arrays children rules in
  let filtered_c_and_r = filter (fun c -> match c with | child, rule -> terminals_match child matcher) c_and_r in
  let rec path_until_found a = 
    match a with
    | [] -> None
    | cr :: rest -> 
      match cr with
      | child, rule ->
        let child_search = if word_terminates child then (if child = matcher then Some [] else None) else get_path child matcher prod in
        match child_search with
        | Some s -> Some (rule :: s)
        | None -> path_until_found rest
  in path_until_found filtered_c_and_r;;

(* Turns terminal characters into leaves and nonterminals into branchless nodes. *)
let rec treeify_rule rule = 
  match rule with
  | [] -> []
  | hd :: tl ->
    match hd with
    | T t -> (Leaf t) :: (treeify_rule tl)
    | N n -> (Node (n, [])) :: (treeify_rule tl)

(* This for some reason only works when rules contains only one element, but what it does is apply the rule to the leftmost empty nonterminal node in the tree. *)
let rec apply_rules_to_tree tree rules =
  match rules with
  | [] -> tree, rules
  | _ ->
    match tree with
    | Leaf l -> (Leaf l), rules
    | Node (n, branches) ->
      match branches with
      | [] -> (Node (n, treeify_rule (List.hd rules))), (List.tl rules)
      | _ ->
        let rec apply_over_branches bs rules =
          match bs with
          | [] -> [], rules
          | br :: rest -> match (apply_rules_to_tree br rules) with
            | new_br, new_rules -> let next = apply_over_branches rest new_rules in
              match next with
              | final_branches, final_rules -> (new_br :: final_branches), final_rules in
      let results = apply_over_branches branches rules in
      match results with
      | updated_branches, updated_rules -> Node(n, updated_branches), updated_rules;;

(* Look its midnight and I missed the deadline by an hour if it works it works, this applies the rules one at a time. *)
let rec shove_those_rules_on tree rules =
  match rules with
  | [] -> tree
  | hd :: tail -> let iteration = apply_rules_to_tree tree [hd] in
    match iteration with
    | tr, rl -> shove_those_rules_on tr tail;;

(* Given a start and a path, build out the tree. *)
let make_parser bad_g = 
  let g = filter_blind_alleys bad_g in
  match g with
  | trigger, prod ->
    fun frag ->
      let path = get_path [N trigger] (terminatize frag) prod in
      match path with
      | None -> None
      | Some p -> Some (shove_those_rules_on (Node (trigger, [])) p);;
  
















