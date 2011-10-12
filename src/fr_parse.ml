
open L_HTML

let rec loop_steps f = function
	| [] -> ()
	| hd :: tl ->
		f hd;
		loop_steps f tl

let step_to_html n r =
	div ~a:[a_class Css_main.Recipe.Step.container_div] [
		div ~a:[a_class Css_main.Recipe.Step.n_div] [
			text (string_of_int n)
		];
		div ~a:[a_class Css_main.Recipe.Step.text_div] [
			text r.T_recipe.Step.text
		];
	]

let steps_to_html l =
	let n = ref 0 in
	List.map (fun step -> incr n; step_to_html !n step) l

let ingridient_to_html r =
	div ~a:[a_class Css_main.Recipe.Ingridient.container_div] [
		text r.T_recipe.Ingridient.name
	]

let ingridients_to_html = List.map ingridient_to_html

let recipe_to_html r =
	div ~a:[a_class Css_main.Recipe.container_div; a_id Common.Recipe.container_div_id] [
		div ~a:[a_class Css_main.Recipe.name_div] [
			text r.T_recipe.name
		];
		div ~a:[a_class Css_main.Recipe.ingridients_div] (ingridients_to_html r.T_recipe.ingridients);
		div ~a:[a_class Css_main.Recipe.steps_div] (steps_to_html r.T_recipe.steps);
	]

let parse s =
	try
		let l = Lexing.from_string s in
		let r = Fr_grammar.input Fr_lexer.recipe l in
		Some r
	with
		| _ -> None

let have_errors s =
	let steps = parse s in
	match steps with
		| Some _ -> false
		| None -> true

let to_html s =
	let r = parse s in
	match r with
		| Some r -> Some (recipe_to_html r)
		| None -> None
