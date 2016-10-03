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
	(* match rules with
	| (start,rhs)::t -> if (start=current_start)
						then (rhs::(recurse start t))
					else []
	| _ -> []  *)

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
	(* rules_to_list_of_alternative_lists None rules *)


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

(* return a matcher for gram *)
let parse_prefix gram = 
	gram



(* let twice (f : int -> int) (x : int) : int = f (f x);; *)
(* let twice f x = f (f x);;
let twice2 f =
	fun x -> f (f x);;
let twice3 (f : int -> int) =
  fun (x : int) -> f (f x);;
let fourth = twice (fun (x : int) -> x * x);;
let fourth2 = twice2 (fun (x : int) -> x * x);; *)



