type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal

let rec contains e l = 
	match l with
	[] -> false
	| h::t when h=e -> true
	| h::t -> (contains e t) 

(* 1 *)
(* true if a is a subset of b. every subset is a subset of itself. 
takes lists of any type *)
let rec subset a b =
	match a with 
	[] -> true
	| h::t -> (contains h b) && (subset t b)

(* 2 *)
(* true if the represented sets are equal *)
let equal_sets a b =
	(subset a b) && (subset b a)


(* rules is a list of pairs of starting symbol and a rhs (list of symbols)
starting symbol is nonterminal such as Expr, Term, etc
list of symbols can be terminal or nonterminal ie [T"("; N Expr; T")" 
Create a production function from the rules.
A production function takes one of the nonterminal symbols ie Expr and returns 
the alternative list for it. The alternative list is a list of all the right hand sides
for the given nonterminal symbol

given this short set of rules...
[ 	Expr, [N Term; N Binop; N Expr];
	Expr, [N Term];
	Term, [N Num];
	Term, [N Lvalue];
]
the production function would be...
	function
	| Expr ->
		[	[N Term; N Binop; N Expr];
			[N Term]
		]
	| Term ->
		[	[N Num];
			[N Lvalue]
		]
*)


let rec recurse_helper current_start rules =
	match rules with
	| (start,rhs)::t -> if (start=current_start)
						then (rhs::(recurse_helper start t))
					else []
	| _ -> [] 

(* return (symbol, symbols alternative list)
what is the empty symbol string???? *)
let rec recurse current_start rules =
	(current_start, recurse_helper current_start rules)

(* 
returns a list of pairs
the pairs consist of a symbol and its alternative list of right hand sides
 *)
let rec rules_to_list_of_alternative_lists current_start rules = 
	match rules with
	| (start,rhs)::t -> if start!=current_start
						then (recurse start rules)::(rules_to_list_of_alternative_lists start t)
						else (rules_to_list_of_alternative_lists start t)
	| _ -> []

let rules_to_list_of_alternative_lists_wrapper rules =
	match rules with
	| (start,rhs)::t -> (recurse start rules)::(rules_to_list_of_alternative_lists start t)
	| _ -> []



let rec alternative_lists_to_production_function list_of_alternative_lists =
	fun input -> match list_of_alternative_lists with
				| (sym,alternative_list)::t -> 	if input=sym
												then alternative_list
												else (alternative_lists_to_production_function t input)
				| _ -> [] 

let convert_rules_to_production_function rules = 
	alternative_lists_to_production_function (rules_to_list_of_alternative_lists_wrapper rules) 

(* grammar from hw1 is a start symbol and rules
grammar for hw2 is a start symbol and a production function
convert rules to a production function *)
let convert_grammar gram1 =
	match gram1 with
	(start, rules) -> (start, convert_rules_to_production_function rules)



let accept_all derivation string = Some (derivation, string)
let accept_empty_suffix derivation = function
   | [] -> Some (derivation, [])
   | _ -> None

(* return a matcher for gram *)
let parse_prefix gram = 
	gram
(* 
returns true if all the symbols in the input are terminal
input is a list of terminal and nonterminal symbols
*)
let rec is_all_terminals input =
	match input with
	| [] -> true
	| h::t -> 	(match h with
				| T s -> (is_all_terminals t)
				| N s -> false)

(* 
return a list of strings
input is a list of terminals symbols
must be all terminal symbols
ex: symbols_to_strings [T"(";T"+";T"-"]-> ["(";"-";"+"]
*)
let rec symbols_to_strings input = 
	if (is_all_terminals input)
		then 	match input with
				| [] -> []
				| h::t -> 	match h with
							| T s -> s::(symbols_to_strings t)
							| _ -> []
	else []

(*  
return true if the potential_prefix is a prefix of the input
potential_prefix is a fragment (list of terminal symbols)
input is also a fragment
*)
let rec is_prefix_of potential_prefix input = 
	match potential_prefix with
	[] -> true
	| h1::t1 ->	match input with
				| [] -> false
				| h2::t2 -> if h1=h2
								then is_prefix_of t1 t2
							else false



(***************)
let rec find_first_nonterminal current_list =
	match current_list with
	| h::t -> 	(match h with
				| N s -> Some s
				| T s -> find_first_nonterminal t)
	| _ -> None

(*
return the alt list of the first nonterminal symbol in current_list 
 *)
let alt_list_of_first_nonterminal p_function current_list = 
	match (find_first_nonterminal current_list) with
	| Some x -> (p_function x)
	| None -> []

(***************)

(*************)
let rec replace_first_nonterminal current_list replacement =
	match current_list with
	| h::t ->	(match h with
				| T s -> h::(replace_first_nonterminal t replacement)
				| N s -> replacement@t
				)
	| _ -> []
(* 
return a list of all the new lists with the first nonterminal symbol replaced
by each rhs in the alternative list
 *)
let rec replace_with_alternatives current_list alt_list =
	match alt_list with
	| h::t -> (replace_first_nonterminal current_list h)::(replace_with_alternatives current_list t)
	| _ -> []

(***************)

let rec possible_prefix_of current_list input =
	match current_list with
	| [] -> true
	| h1::t1 -> match h1 with
				| N s -> true
				| T s -> 	match input with 
							| [] -> false
							| h2::t2 ->	if h1=h2
											then possible_prefix_of t1 t2
										else false
(*  *)
(* 
if the current list is all terminals then nothing to replace and return it
else if there is a nonterminal and its length is less than or equal to the length
of the input then replace nonterminal with its alternatives. if the length is
greater then return empty list
 *)
let rec filter_lists list_of_lists input =
	match list_of_lists with
	| [] -> []
	| h::t -> 	if (possible_prefix_of h input)
					then h::(filter_lists t input)
 				else (filter_lists t input)

let replace_nonterminal_with_alternatives p_function current_list input =
	if (is_all_terminals current_list)
		then [current_list]
	else if ((List.length current_list) <= (List.length input)) 
		then ( filter_lists (replace_with_alternatives current_list (alt_list_of_first_nonterminal p_function current_list)) input)
		(* then (  (replace_with_alternatives current_list (alt_list_of_first_nonterminal p_function current_list)) ) *)
	else []

(* 
list_of_lists_of_symbols is a list of the possible results when you replace the first nonterminal symbol with rhs's
from its alternative list
ex: [N Term; N Binop] where N Term -> [[N Num];[N Lvalue]] would give [ [N Num;N Binop]; [N Lvalue; N Binop]]
 *)
let rec find_all_fragments p_function list_of_lists_of_symbols input =
	(* match list_of_lists_of_symbols with 
	| h::t -> (tobenamed_helper p_function h input)@t
	| _ -> [] *)

	match list_of_lists_of_symbols with 
	| h::t -> 	if (is_all_terminals h)
					then h::(find_all_fragments p_function t input)
				else (find_all_fragments p_function ((replace_nonterminal_with_alternatives p_function h input)@t) input)
	| _ -> []

let find_all_fragments_wrapper gram input =
	find_all_fragments (snd gram) ( [[N (fst gram)]]) input
	

(* 
returns the first prefix that matches and is accpeted  
(possible alternate - or should i return a list of prefixes and check later)
current_list is a list of nonterminal and terminal symbols
look for the first non terminal and replace based on leftmost derivation 
*)
(* let find_all_prefixes_helper p_function current_list input =
	match current_list with
	| h::t -> 	match h with
				if h is nonterminal, then replace it with the first item in its alternative list
				| N s -> (find_all_prefixes_helper p_function (p_function h)  ) 
				then find_all_prefixes_helper gram (p_function h)@t *)


(* test rules left-to-right. see if the terminable state is a prefix for the given 
list of terminables *)
	(* put nonterminal start symbol into production function to get alternative 
	list. use the first substitution and recurse. then try next ones *)

	(* (snd gram) (fst gram) gets the alternative list for the start symbol
	take the first item in the list and replace its first nonterminal symbol
	and recurse. then go to the next item in the list
	stop recursing if the length of the list gets longer than the input
	 *)
(* 	 
let find_all_prefixes gram input =
	match ((snd gram) (fst gram)) with
	| h::t -> 	match (find_all_prefixes_helper (snd gram) h input) with
				| Some x -> x
				| None -> find_all_prefixes_helper (snd gram) t input

 *)	


(* matcher finds prefix matches and then checks them by testing if acceptor 
succeeds on the corresponding derivation and suffix
i think the derivation is a list of rules that lead to the prefix *)
(* let matcher acceptor fragment =
	acceptor
let create_matcher something =
	fun acceptor fragment -> matcher acceptor fragment *)


(* let twice (f : int -> int) (x : int) : int = f (f x);; *)
(* let twice f x = f (f x);;
let twice2 f =
	fun x -> f (f x);;
let twice3 (f : int -> int) =
  fun (x : int) -> f (f x);;
let fourth = twice (fun (x : int) -> x * x);;
let fourth2 = twice2 (fun (x : int) -> x * x);; *)



