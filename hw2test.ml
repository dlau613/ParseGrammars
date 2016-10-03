type awksub_nonterminals =
  | Expr | Term | Lvalue | Incrop | Binop | Num 

let awksub_rules =
   [Expr, [N Term; N Binop; N Expr];
    Expr, [N Term];
    Term, [N Num];
    Term, [N Lvalue];
    Term, [N Incrop; N Lvalue];
    Term, [N Lvalue; N Incrop];
    Term, [T"("; N Expr; T")"];
    Lvalue, [T"$"; N Expr];
    Incrop, [T"++"];
    Incrop, [T"--"];
    Binop, [T"+"];
    Binop, [T"-"];
    Num, [T"0"];
    Num, [T"1"];
    Num, [T"2"];
    Num, [T"3"];
    Num, [T"4"];
    Num, [T"5"];
    Num, [T"6"];
    Num, [T"7"];
    Num, [T"8"];
    Num, [T"9"]]

let recurse_test = 
	equal_sets (recurse_helper Expr awksub_rules) [[N Term; N Binop; N Expr]; [N Term]]

let recurse_test2 = 
	equal_sets (recurse_helper Term (List.tl (List.tl awksub_rules))) [ [N Num]; [N Lvalue]; [
		N Incrop; N Lvalue]; [N Lvalue; N Incrop]; [T"("; N Expr; T")"] ] 

let rules_to_alternative_lists_test = 
	rules_to_alternative_lists Num awksub_rules

let simple = 
	simple (Expr, [ [N Term; N Binop; N Expr]; [N Term] ] )
(*
let awksub_grammar = Expr, awksub_rules

let awksub_grammar_2 = convert_grammar awksub_grammar;; *)
(* 
let awkish_grammar = 
	(Expr,
   function
     | Expr ->
         [[N Term; N Binop; N Expr];
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
	  [T"5"]; [T"6"]; [T"7"]; [T"8"]; [T"9"]]);;
 *)
(* let testing = 
	(snd awkish_grammar) Binop;; *)





