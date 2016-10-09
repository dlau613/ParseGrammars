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

let convert_grammar_test0 = 
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
	(snd awkish_grammar) Num= (snd awksub_grammar_converted) Num

 
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

let alt_list_of_first_nonterminal_test0 = 
	alt_list_of_first_nonterminal (snd awkish_grammar) [T"(";N Binop; N Expr] = [[T "+"]; [T "-"]]

let alt_list_of_first_nonterminal_test1 = 
	alt_list_of_first_nonterminal (snd awkish_grammar) [N Term;N Binop; N Expr] = [[N Num];
	  [N Lvalue]; [N Incrop; N Lvalue];[N Lvalue; N Incrop];[T"("; N Expr; T")"]]

let alt_list_of_first_nonterminal_test2 = 
	alt_list_of_first_nonterminal (snd awkish_grammar) [T"(";T"+";T")"] = []

(* let replace_first_nonterminal_helper_test0 =
	replace_first_nonterminal_helper [N Term; N Binop; N Expr] [N Num] *)
let replace_first_nonterminal_test0 =
	replace_first_nonterminal ([(Expr,[N Term; N Binop; N Expr])], [N Term; N Binop; N Expr]) [N Num] =([(Expr, [N Term; N Binop; N Expr]); (Term, [N Num])],
   [N Num; N Binop; N Expr])

let replace_first_nonterminal_test1 =
	replace_first_nonterminal ( [(Expr,[N Term; N Binop; N Expr]);(Term, [N Num])], [T"0";T"+";N Expr]) [N Term;N Binop; N Expr] =
	([(Expr, [N Term; N Binop; N Expr]); (Term, [N Num]);
    (Expr, [N Term; N Binop; N Expr])],
   [T "0"; T "+"; N Term; N Binop; N Expr])

let replace_with_alternatives_test0 =
	replace_with_alternatives ([ (Expr, [N Term;N Binop;N Expr]); (Term, [N Num])] , [N Term; N Binop; N Expr]) [[N Num];[N Lvalue];[N Incrop; N Lvalue];
	  [N Lvalue; N Incrop];[T"("; N Expr; T")"]]= [([(Expr, [N Term; N Binop; N Expr]); (Term, [N Num]); (Term, [N Num])],
    [N Num; N Binop; N Expr]);
   ([(Expr, [N Term; N Binop; N Expr]); (Term, [N Num]); (Term, [N Lvalue])],
    [N Lvalue; N Binop; N Expr]);
   ([(Expr, [N Term; N Binop; N Expr]); (Term, [N Num]);
     (Term, [N Incrop; N Lvalue])],
    [N Incrop; N Lvalue; N Binop; N Expr]);
   ([(Expr, [N Term; N Binop; N Expr]); (Term, [N Num]);
     (Term, [N Lvalue; N Incrop])],
    [N Lvalue; N Incrop; N Binop; N Expr]);
   ([(Expr, [N Term; N Binop; N Expr]); (Term, [N Num]);
     (Term, [T "("; N Expr; T ")"])],
    [T "("; N Expr; T ")"; N Binop; N Expr])]

let replace_with_alternatives_test1 =
	replace_with_alternatives ([],[T"3";N Binop; N Expr]) [[T"+"];[T"-"]] = [([(Binop, [T "+"])], [T "3"; T "+"; N Expr]);
   ([(Binop, [T "-"])], [T "3"; T "-"; N Expr])]


let possible_prefix_of_test0 = 
	possible_prefix_of [T"3"] [T"3";T"+";T"4";T"-"]

let possible_prefix_of_test1 = 
	possible_prefix_of [T"3";N Expr] [T"3";T"+";T"4";T"-"]

let possible_prefix_of_test2 = 
	not (possible_prefix_of [T"1";N Expr] [T"3";T"+";T"4";T"-"])

let find_fragments_wrapper_test0 = 
	find_fragments_wrapper awkish_grammar [T"3";T"+";T"4";T"-"] 
	= [([(Expr, [N Term; N Binop; N Expr]); (Term, [N Num]); (Num, [T "3"]);
     (Binop, [T "+"]); (Expr, [N Term]); (Term, [N Num]); (Num, [T "4"])],
    [T "3"; T "+"; T "4"]);
   ([(Expr, [N Term]); (Term, [N Num]); (Num, [T "3"])], [T "3"])]

(* let find_fragments_wraper_test1 =
	find_fragments_wrapper awkish_grammar2 [T"3";T"+";T"4";T"-"] 
	= [([(Expr, [N Term; N Binop; N Expr]); (Term, [N Num]); (Num, [T "3"]);
     (Binop, [T "+"]); (Expr, [N Term]); (Term, [N Num]); (Num, [T "4"])],
    [T "3"; T "+"; T "4"]);
   ([(Expr, [N Term]); (Term, [N Num]); (Num, [T "3"])], [T "3"])] *)



let find_corresponding_suffix_helper_test0 = 
	find_corresponding_suffix_helper [T"3";T"+";T"4"] [T"3";T"+";T"4";T"-"] = [T "-"]

let find_corresponding_suffix_helper_test1 = 
	find_corresponding_suffix_helper [T"3"] [T"3";T"+";T"4";T"-"] = [T"+";T"4";T"-"]

let find_corresponding_suffix_test0 = 
	find_corresponding_suffix [([(Expr, [N Term; N Binop; N Expr]); (Term, [N Num]); (Num, [T "3"]);
     (Binop, [T "+"]); (Expr, [N Term]); (Term, [N Num]); (Num, [T "4"])],
    [T "3"; T "+"; T "4"]);
   ([(Expr, [N Term]); (Term, [N Num]); (Num, [T "3"])], [T "3"])] [T"3";T"+";T"4";T"-"] =
    [([(Expr, [N Term; N Binop; N Expr]); (Term, [N Num]); (Num, [T "3"]);
     (Binop, [T "+"]); (Expr, [N Term]); (Term, [N Num]); (Num, [T "4"])],
    ["-"]);
   ([(Expr, [N Term]); (Term, [N Num]); (Num, [T "3"])], ["+"; "4"; "-"])]

let find_derivations_and_suffixes_test0 = 
	find_derivations_and_suffixes awkish_grammar [T"3";T"+";T"4";T"-"]
	= [([(Expr, [N Term; N Binop; N Expr]); (Term, [N Num]); (Num, [T "3"]);
     (Binop, [T "+"]); (Expr, [N Term]); (Term, [N Num]); (Num, [T "4"])],
    ["-"]);
   ([(Expr, [N Term]); (Term, [N Num]); (Num, [T "3"])], ["+"; "4"; "-"])]

let parse_prefix_helper_test0 = 
	parse_prefix_helper [([(Expr, [N Term; N Binop; N Expr]); (Term, [N Num]); (Num, [T "3"]);
     (Binop, [T "+"]); (Expr, [N Term]); (Term, [N Num]); (Num, [T "4"])],
    [T "-"]);
   ([(Expr, [N Term]); (Term, [N Num]); (Num, [T "3"])],
    [T "+"; T "4"; T "-"])] accept_all
   = Some
   ([(Expr, [N Term; N Binop; N Expr]); (Term, [N Num]); (Num, [T "3"]);
     (Binop, [T "+"]); (Expr, [N Term]); (Term, [N Num]); (Num, [T "4"])],
    [T "-"])

(* Test Cases from Spec *)
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

(* This one might take a bit longer.... *)
let test4 =
 ((parse_prefix awkish_grammar accept_all
     ["("; "$"; "8"; ")"; "-"; "$"; "++"; "$"; "--"; "$"; "9"; "+";
      "("; "$"; "++"; "$"; "2"; "+"; "("; "8"; ")"; "-"; "9"; ")";
      "-"; "("; "$"; "$"; "$"; "$"; "$"; "++"; "$"; "$"; "5"; "++";
      "++"; "--"; ")"; "-"; "++"; "$"; "$"; "("; "$"; "8"; "++"; ")";
      "++"; "+"; "0"])
  = Some
     ([(Expr, [N Term; N Binop; N Expr]); (Term, [T "("; N Expr; T ")"]);
       (Expr, [N Term]); (Term, [N Lvalue]); (Lvalue, [T "$"; N Expr]);
       (Expr, [N Term]); (Term, [N Num]); (Num, [T "8"]); (Binop, [T "-"]);
       (Expr, [N Term; N Binop; N Expr]); (Term, [N Lvalue]);
       (Lvalue, [T "$"; N Expr]); (Expr, [N Term; N Binop; N Expr]);
       (Term, [N Incrop; N Lvalue]); (Incrop, [T "++"]);
       (Lvalue, [T "$"; N Expr]); (Expr, [N Term; N Binop; N Expr]);
       (Term, [N Incrop; N Lvalue]); (Incrop, [T "--"]);
       (Lvalue, [T "$"; N Expr]); (Expr, [N Term; N Binop; N Expr]);
       (Term, [N Num]); (Num, [T "9"]); (Binop, [T "+"]); (Expr, [N Term]);
       (Term, [T "("; N Expr; T ")"]); (Expr, [N Term; N Binop; N Expr]);
       (Term, [N Lvalue]); (Lvalue, [T "$"; N Expr]);
       (Expr, [N Term; N Binop; N Expr]); (Term, [N Incrop; N Lvalue]);
       (Incrop, [T "++"]); (Lvalue, [T "$"; N Expr]); (Expr, [N Term]);
       (Term, [N Num]); (Num, [T "2"]); (Binop, [T "+"]); (Expr, [N Term]);
       (Term, [T "("; N Expr; T ")"]); (Expr, [N Term]); (Term, [N Num]);
       (Num, [T "8"]); (Binop, [T "-"]); (Expr, [N Term]); (Term, [N Num]);
       (Num, [T "9"]); (Binop, [T "-"]); (Expr, [N Term]);
       (Term, [T "("; N Expr; T ")"]); (Expr, [N Term]); (Term, [N Lvalue]);
       (Lvalue, [T "$"; N Expr]); (Expr, [N Term]); (Term, [N Lvalue]);
       (Lvalue, [T "$"; N Expr]); (Expr, [N Term]); (Term, [N Lvalue]);
       (Lvalue, [T "$"; N Expr]); (Expr, [N Term]); (Term, [N Lvalue; N Incrop]);
       (Lvalue, [T "$"; N Expr]); (Expr, [N Term]); (Term, [N Lvalue; N Incrop]);
       (Lvalue, [T "$"; N Expr]); (Expr, [N Term]); (Term, [N Incrop; N Lvalue]);
       (Incrop, [T "++"]); (Lvalue, [T "$"; N Expr]); (Expr, [N Term]);
       (Term, [N Lvalue; N Incrop]); (Lvalue, [T "$"; N Expr]); (Expr, [N Term]);
       (Term, [N Num]); (Num, [T "5"]); (Incrop, [T "++"]); (Incrop, [T "++"]);
       (Incrop, [T "--"]); (Binop, [T "-"]); (Expr, [N Term]);
       (Term, [N Incrop; N Lvalue]); (Incrop, [T "++"]);
       (Lvalue, [T "$"; N Expr]); (Expr, [N Term]); (Term, [N Lvalue; N Incrop]);
       (Lvalue, [T "$"; N Expr]); (Expr, [N Term]);
       (Term, [T "("; N Expr; T ")"]); (Expr, [N Term]);
       (Term, [N Lvalue; N Incrop]); (Lvalue, [T "$"; N Expr]); (Expr, [N Term]);
       (Term, [N Num]); (Num, [T "8"]); (Incrop, [T "++"]); (Incrop, [T "++"]);
       (Binop, [T "+"]); (Expr, [N Term]); (Term, [N Num]); (Num, [T "0"])],
      []))


let rec contains_lvalue = function
  | [] -> false
  | (Lvalue,_)::_ -> true
  | _::rules -> contains_lvalue rules

let accept_only_non_lvalues rules frag =
  if contains_lvalue rules
  then None
  else Some (rules, frag)

let test5 =
  ((parse_prefix awkish_grammar accept_only_non_lvalues
      ["3"; "-"; "4"; "+"; "$"; "5"; "-"; "6"])
   = Some
      ([(Expr, [N Term; N Binop; N Expr]); (Term, [N Num]); (Num, [T "3"]);
	(Binop, [T "-"]); (Expr, [N Term]); (Term, [N Num]); (Num, [T "4"])],
       ["+"; "$"; "5"; "-"; "6"]))

(* MY TESTS *)
(* added Expr -> [N Expr; N Term; N Binop] to test non-tail recursion  *)
let awkish_grammar2 = 
	(Expr,
   function
     | Expr -> 
         [[N Expr; N Term; N Binop];[N Term;N Binop;N Expr];
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

let test_1 =
  ((parse_prefix awkish_grammar2 accept_all ["9"; "+"; "$"; "1"; "+"])
   = Some
       ([(Expr, [N Term; N Binop; N Expr]); (Term, [N Num]); (Num, [T "9"]);
	 (Binop, [T "+"]); (Expr, [N Term]); (Term, [N Lvalue]);
	 (Lvalue, [T "$"; N Expr]); (Expr, [N Term]); (Term, [N Num]);
	 (Num, [T "1"])],
	["+"]))

type awksub_nonterminals =
  | Expr | Term | Lvalue | Incrop | Binop | Num | Temp

(* adding Expr -> [N Expr] tests for when a nonterminal is continuously replaced byitself 
adding Expr -> [N Temp; N Expr] and Temp -> [N Temp] tests for blind alley*)
let awkish_grammar3 = 
	(Expr,
   function
     | Expr -> 
         [[N Expr; N Term; N Binop] ;[N Temp; N Expr];[N Expr];[N Term;N Binop;N Expr];
          [N Term]]
     | Temp -> 
     	 [[ N Temp]]
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

let test_2 =
  ((parse_prefix awkish_grammar3 accept_all ["9"; "+"; "$"; "1"; "+"])
  	= Some
       ([(Expr, [N Term; N Binop; N Expr]); (Term, [N Num]); (Num, [T "9"]);
	 (Binop, [T "+"]); (Expr, [N Term]); (Term, [N Lvalue]);
	 (Lvalue, [T "$"; N Expr]); (Expr, [N Term]); (Term, [N Num]);
	 (Num, [T "1"])],
	["+"]))
