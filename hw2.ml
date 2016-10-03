type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal

(* grammar from hw1 is a start symbol and rules
grammar for hw2 is a start symbol and a production function
convert rules to a production function *)
(* let convert_grammar gram1 =
	match gram1 with
	(start, rules) -> (start, convert_rules_to_production_function rules)
 *)
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

(* let convert_rules_to_production_function rules =
	match rules with
	| (start,rhs)::t -> *)

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

initial call should have current_start as None?
 *)
let rec rules_to_alternative_lists current_start rules = 
	match rules with
	| (start,rhs)::t -> if start!=current_start 
						then (recurse start rules)::(rules_to_alternative_lists start t)
						else (rules_to_alternative_lists start t)
	| _ -> []

let rec simple (sym, alternative_list) = 
	fun sym -> sym

(* let twice (f : int -> int) (x : int) : int = f (f x);; *)
(* let twice f x = f (f x);;
let twice2 f =
	fun x -> f (f x);;
let twice3 (f : int -> int) =
  fun (x : int) -> f (f x);;
let fourth = twice (fun (x : int) -> x * x);;
let fourth2 = twice2 (fun (x : int) -> x * x);; *)


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

