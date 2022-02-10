(* HOMEWORK 1 *)

(* QUESTION 1 *)

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



(* QUESTION 2 *)

let equal_sets a b =
  (subset a b) && (subset b a);;
  


(* QUESTION 3 *)

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
  


(* QUESTION 4 *)

let rec set_all_union a =
  match a with
  | [] -> []
  | [last] -> last
  | hd :: tl ->
    set_all_union (((set_union hd (List.hd tl))) :: (List.tl tl));;



(* QUESTION 5 *)

(** We cannot write self_member s, as self_member s = is_member s s, but is_member a b 
takes a type of data and a list of that type of data, which in OCaml's strongly typed 
language causes an error*)



(* QUESTION 6 *)

let f x = 3.2 *. x *. (1. -. x);;

let rec computed_fixed_point eq f x = 
  if eq x (f x) then x
  else computed_fixed_point eq f (f x);;



(* QUESTION 7 *)

let rec recurse f p x = if p <= 0 then x else f (recurse f (p-1) x);;

let rec computed_periodic_point eq f p x =
  if eq x (recurse f p x) then x
  else computed_periodic_point eq f p (f x);;



(* QUESTION 8 *)

let rec whileseq s p x =
  if p x then x :: (whileseq s p (s x)) else [];;



(* QUESTION 9 *)

type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal;;

(* Quick way to check if given symbol is of type N or T. *)
let rec is_terminal (s : ('nonterminal, 'terminal) symbol list) = 
  match s with
  | [] -> []
  | hd :: tl ->
    match hd with
    | N n -> (n, false) :: is_terminal tl
    | T t -> is_terminal tl;;

(* Useful for some debugging. *)
let get_rules g = 
  match g with start, rules -> rules;;

(* Takes in grammar, outputs list of tuples (symbol, bool) for each non-terminating symbol in the language and whether or not it's terminal. This is our starting condition. *)
let rec get_characters g = 
  match g with
  | start, rules -> 
    match rules with
      | [] -> []
      | rule :: rest -> 
          match rule with
            | trigger, output -> set_union (is_terminal output) (get_characters (start, rest));;

(* Checks in the dictionary if a symbol is marked as terminating eventually or not. *)
let rec symbol_terms terms symbol =
  match symbol with
  | T t -> true
  | N letter ->
    match terms with
    | [] -> true
    | term :: rest ->
      match term with
      | l, status -> if l = letter then status && (symbol_terms rest (N l)) else symbol_terms rest (N letter);;

(* Checks if a word is only composed of symbols marked as terminating in the dictionary. *)
let rec word_terms terms word =
  match word with
  | [] -> true
  | symbol :: rest -> (symbol_terms terms symbol) && (word_terms terms rest);;

(* Flip a symbol in the dictionary from being marked as nonterminating to terminating. *)
let rec mark_as_terminating letter dict =
  match dict with
  | [] -> []
  | term :: rest ->
    match term with
    | l, status -> if l = letter then (l, true) :: (mark_as_terminating letter rest)
    else (l, status) :: (mark_as_terminating letter rest);;

(* Loops over all rules, marking as terminating the trigger symbol of a rule if its output word only consists of terminating symbols. *)
let rec iterate_terminals terms rules =
  match rules with
  | [] -> terms
  | rule :: rest ->
    match rule with
    | trigger, output -> if word_terms terms output then iterate_terminals (mark_as_terminating trigger terms) rest
    else iterate_terminals terms rest;;

(* Loops iterate_terminals until no more symbols are being marked as terminating - now any still marked as non-terminating will never terminate. *)
let get_terminateables g = 
  match g with 
  | start, rules ->
    computed_fixed_point equal_sets (fun dict -> iterate_terminals dict rules) (get_characters g);;

(* After getting the true termination status of all symbols, prune any rules containing a non-terminating symbol. *)
let rec filter_rules rules dict =
  match rules with
  | [] -> []
  | rule :: rest ->
    match rule with
    | trigger, output ->
      if (symbol_terms dict (N trigger)) && (word_terms dict output) 
        then rule :: filter_rules rest dict
      else filter_rules rest dict;;

(* Splice filtered rules into the given grammar. *)
let filter_blind_alleys g =
  let terminateables = get_terminateables g in
  match g with
    start, rules -> start, (filter_rules rules terminateables);;

  


