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

let rules_to_list_of_alternative_lists_wrapper_test = 
	equal_sets (rules_to_list_of_alternative_lists_wrapper awksub_rules) [(Expr, [[N Term; N Binop; N Expr]; [N Term]]);
   (Term,
    [[N Num]; [N Lvalue]; [N Incrop; N Lvalue]; [N Lvalue; N Incrop];
     [T "("; N Expr; T ")"]]);
   (Lvalue, [[T "$"; N Expr]]); (Incrop, [[T "++"]; [T "--"]]);
   (Binop, [[T "+"]; [T "-"]]);
   (Num,
    [[T "0"]; [T "1"]; [T "2"]; [T "3"]; [T "4"]; [T "5"]; [T "6"]; [T "7"];
     [T "8"]; [T "9"]])]


let convert_rules_to_production_function_test =
	(convert_rules_to_production_function awksub_rules) Binop


let awksub_grammar = Expr, awksub_rules

let awksub_grammar_converted = 
	convert_grammar awksub_grammar

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

(* let convert_grammar_test0 = 
	(snd awkish_grammar) Binop = (snd awksub_grammar_converted) Binop

let convert_grammar_test1 = 
	(snd awkish_grammar) Expr = (snd awksub_grammar_converted) Expr

let convert_grammar_test2 = 
	(snd awkish_grammar) Term= (snd awksub_grammar_converted) Term

let convert_grammar_test3 = 
	(snd awkish_grammar) Lvalue = (snd awksub_grammar_converted) Lvalue

let convert_grammar_test4 = 
	(snd awkish_grammar) Incrop= (snd awksub_grammar_converted) Incrop

let convert_grammar_test5 = 
	(snd awkish_grammar) Num= (snd awksub_grammar_converted) Num *)

(* 
let is_all_terminals_test0 = 
	not (is_all_terminals [N Term; N Binop; N Expr])

let is_all_terminals_test1 =
	is_all_terminals [T"(";T"+";T"-"]

let is_all_terminals_test2 =
	not (is_all_terminals [T"(";N Binop;T"-"])

let is_all_terminals_test3 =
	is_all_terminals []


let symbols_to_strings_test0 = 
	equal_sets (symbols_to_strings [T"(";T"+";T"-"]) ["(";"-";"+"]

let symbols_to_strings_test1 = 
	symbols_to_strings [T"+";N Expr;T"-"] = [] 	

let is_prefix_of_test0 = 
	is_prefix_of ["3";"+";"4"] ["3";"+";"4";"-"]

let is_prefix_of_test1 = 
	is_prefix_of ["3"] ["3";"+";"4";"-"]

let is_prefix_of_test2 = 
	not (is_prefix_of ["+"] ["3";"+";"4";"-"])

let find_first_nonterminal_test0 = 
	find_first_nonterminal [N Term; N Binop; N Expr] = Some Term

let find_first_nonterminal_test1 = 
	find_first_nonterminal [T"("; N Binop; N Expr] = Some Binop

let find_first_nonterminal_test2 = 
	find_first_nonterminal [T"("; T"+"] = None

let find_first_nonterminal_test3 =
	find_first_nonterminal [] = None
*)
let alt_list_of_first_nonterminal_test0 = 
	alt_list_of_first_nonterminal (snd awkish_grammar) [T"(";N Binop; N Expr] = [[T "+"]; [T "-"]]

let alt_list_of_first_nonterminal_test1 = 
	alt_list_of_first_nonterminal (snd awkish_grammar) [N Term;N Binop; N Expr] = [[N Num];
	  [N Lvalue]; [N Incrop; N Lvalue];[N Lvalue; N Incrop];[T"("; N Expr; T")"]]

let alt_list_of_first_nonterminal_test0 = 
	alt_list_of_first_nonterminal (snd awkish_grammar) [T"(";T"+";T")"] = []

let replace_first_nonterminal_test0 =
	(replace_first_nonterminal [N Term; N Binop; N Expr] [N Num]) = [N Num; N Binop; N Expr]

let replace_first_nonterminal_test1 =
	(replace_first_nonterminal [T")"; T"+"; N Expr] [T"-";N Binop]) = [T")";T"+";T"-";N Binop]

let replace_with_alternatives_test0 =
	replace_with_alternatives [N Term; N Binop; N Expr] [[N Num];[N Lvalue];[N Incrop; N Lvalue];
	  [N Lvalue; N Incrop];[T"("; N Expr; T")"]] = [[N Num; N Binop; N Expr]; [N Lvalue; N Binop; N Expr];
	  [N Incrop; N Lvalue; N Binop; N Expr]; [N Lvalue; N Incrop; N Binop; N Expr]; [T"(";N Expr; T")"; N Binop; N Expr]]

let replace_with_alternatives_test1 =
	replace_with_alternatives [T"3";N Binop; N Expr] [[T"+"];[T"-"]] = [ [T"3"; T"+";N Expr]; [T"3";T"-";N Expr]]

let possible_prefix_of_test0 = 
	possible_prefix_of [T"3"] [T"3";T"+";T"4";T"-"]

let possible_prefix_of_test1 = 
	possible_prefix_of [T"3";N Expr] [T"3";T"+";T"4";T"-"]

let possible_prefix_of_test2 = 
	not (possible_prefix_of [T"1";N Expr] [T"3";T"+";T"4";T"-"])

let mini_grammar = 
	(Expr,
   	function
     | Expr ->
         [[N Term; N Binop; N Expr];
          [N Term]]
     | Term ->
	 [[N Num];
	  [N Lvalue]];
     | Lvalue ->
	 [[T"$"; N Expr]]
     | Incrop ->
	 [[T"++"];
	  [T"--"]]
     | Binop ->
	 [[T"+"];
	  [T"-"]]
     | Num ->
	 [[T"0"]; [T"1"]]);;

let replace_nonterminal_with_alternatives_test0 = 
	replace_nonterminal_with_alternatives (snd mini_grammar) [N Expr] [T"3";T"+";T"4"] = [[N Term; N Binop; N Expr]; [N Term]]

let replace_nonterminal_with_alternatives_test1 = 
	replace_nonterminal_with_alternatives (snd mini_grammar) [N Expr] [] = []


let find_all_fragments_test0 = 
	find_all_fragments (snd mini_grammar) [[N Expr]] [T"3";T"+";T"4"]

let find_all_fragments_wrapper_test0 = 
	find_all_fragments_wrapper awkish_grammar [T"3";T"+";T"4";T"-"] 






(* let tobenamed_test1 = 
	tobenamed (snd mini_grammar) [[N Term;N Binop; N Expr];[N Term]] [T"3";T"+"] *)

(* let temp_test = 
	tobenamed (snd mini_grammar) [[N Term; N Binop; N Expr]; [N Term]] [T"3";T"+";T"4"]

let temp_test2 =
	tobenamed (snd mini_grammar) [[N Num; N Binop; N Expr]; [N Lvalue; N Binop; N Expr]; [N Term]] [T"3";T"+";T"4"] *)
(* 
let find_all_prefixes_test = 
	find_all_prefixes awkish_grammar 2 3
let test0 =
  ((parse_prefix awkish_grammar accept_all ["ouch"]) = None)

let test1 =
  ((parse_prefix awkish_grammar accept_all ["9"])
   = Some ([(Expr, [N Term]); (Term, [N Num]); (Num, [T "9"])], []))

let test2 =
  ((parse_prefix awkish_grammar accept_all ["9"; "+"; "$"; "1"; "+"])
   = Some
       ([(Expr, [N Term; N Binop; N Expr]); (Term, [N Num]); (Num, [T "9"]);
	 (Binop, [T "+"]); (Expr, [N Term]); (Term, [N Lvalue]);
	 (Lvalue, [T "$"; N Expr]); (Expr, [N Term]); (Term, [N Num]);
	 (Num, [T "1"])],
	["+"]))

let test3 =
  ((parse_prefix awkish_grammar accept_empty_suffix ["9"; "+"; "$"; "1"; "+"])
   = None)

 *)

