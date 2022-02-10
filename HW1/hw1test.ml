(* TEST CASES *)

(* My test case, with grammar g. 0 and 1 can terminate, but 4 and 5 cannot.*)
let r1 = 0, [N 1;N 1;N 1];;
let r2 = 1, [T 'd'];;
let r3 = 1, [N 0; T 'c'];;
let r4 = 1, [N 0];;
let r5 = 1, [N 4];;
let r6 = 4, [N 5];;
let r7 = 5, [N 4];;

let g = 0, [r1; r2; r3; r4; r5; r6; r7];;



let my_subset_test0 = 
  (subset [1;3;5] [3;2;1;5;4]);;

let my_subset_test1 = 
    (subset [] []);;

let my_subset_test2 = 
  not (subset [1;3;4] [3;1]);;


let my_equal_sets_test0 = 
  equal_sets [0;1;2] [2;1;0;1];;

let my_equal_sets_test1 = 
  equal_sets [] [];;


let my_set_union_test0 =
  equal_sets (set_union [0; 1; 1] [2; 3; 0; 1]) [0; 1; 2; 3];;

let my_set_union_test1 =
  equal_sets (set_union [0; 1; 1; 1] []) [0; 1];;


let my_set_all_union_test0 =
  equal_sets (set_all_union [[0;1];[1;2];[2;3];[3;0];[4]]) [0;1;2;3;4];;

let my_set_all_union_test1 =
  equal_sets (set_all_union []) [];;


let f (x : float) = 2.0 *. x *. x -. 1.0;;

let my_computed_fixed_point_test0 = 
  computed_fixed_point (=) (fun (x : float) -> x *. x) 0.8 = 0.;;

let my_computed_fixed_point_test1 = 
  computed_fixed_point (=) f (0.8) = 1.0;; 


let periodic_function x =
  match x with
  | 0 -> 1
  | 1 -> 2
  | 2 -> 3
  | _ -> 2;;

let my_computed_periodic_point_test0 = 
  computed_periodic_point (=) periodic_function 2 0 = 2;; 


let my_whileseq_test0 =
  whileseq (( * ) 2) ((>) 100) 1 = [1; 2; 4; 8; 16; 32; 64];;


let my_filter_blind_alleys_test0 = 
  filter_blind_alleys g = (0, [r1; r2; r3; r4]);;
