type my_nonterminals =
  | Expr | Term | Lvalue | Incrop | Binop | Num | BlindAlley1 | BlindAlley

(* This grammar is non-trivial because it will try to immediately go down a blind alley, and also Num can also just self destruct *)
let blindalley_grammar =
  (Expr,
    function
      | Expr ->
          [[N BlindAlley1];
          [N Term; N Binop; N Expr];
          [N Term]]
      | Term ->
    [[N Num];
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
    [T"5"]; [T"6"]; [T"7"]; [T"8"]; [T"9"]; [] (* Note the extra [] *)]
      | BlindAlley1 ->
    [[T"+";N BlindAlley1]; [N BlindAlley]]
      | BlindAlley ->
    [[T"+";N BlindAlley]; [N BlindAlley1]])

(* Since Num can go to an empty string, now Expr -> Term Binop Expr -> Term Binop Term Binop Term -> Num Binop Lvalue Binop Num
    -> 9 + $ Expr + (notice how the last Num self destructs) -> 9 + $ 1 +    *)
let make_matcher_test =
  ((make_matcher blindalley_grammar accept_all ["9"; "+"; "$"; "1"; "+"])
   = Some [])

(* Even tougher; make a num in the middle of the string disappear as well. *)
let make_parser_test = 
  let parse_tree = make_parser blindalley_grammar ["9"; "+"; "$"; "+"] in
  match parse_tree with
  | Some tree -> parse_tree_leaves tree = ["9"; "+"; "$"; "+"]
  | _ -> false