
open L_HTML

let rec loop_steps f = function
	| [] -> ()
	| hd :: tl ->
		f hd;
		loop_steps f tl

(*
let rec to_html n = function
	| [] -> []
	| step :: tl ->
		let step =
			div ~a:[a_class Css_main.Recipe.Parts.part_div] [
				div ~a:[a_class Css_main.Recipe.Parts.part_number] [
					text ((string_of_int n) ^ ".")
				];
				text step.T_recipe.Step.text
			]
		in
		step :: to_html (n+1) tl
*)
(*
let to_format l =
	let b = Buffer.create 16380 in
	loop_steps (fun step ->
		Buffer.add_string b "step {\n\t";
		Buffer.add_string b step.T_recipe.Step.text;
		Buffer.add_string b "}\n"
	) l;
	Buffer.contents b
*)

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

let recipe_to_html r =
	div ~a:[a_class Css_main.Recipe.container_div; a_id Common.Recipe.container_div_id] [
		div ~a:[a_class Css_main.Recipe.name_div] [
			text r.T_recipe.Recipe.name
		];
		div ~a:[a_class Css_main.Recipe.steps_div] (steps_to_html r.T_recipe.Recipe.steps)
	]

let component_to_html r =
	div ~a:[a_class Css_main.Component.content_div; a_id Common.Recipe.container_div_id] [
		div ~a:[a_class Css_main.Component.name_div] [
			text r.T_recipe.Component.name
		];
	]

let result_to_html = function
	| T_recipe.Recipe r -> recipe_to_html r
	| T_recipe.Component r -> component_to_html r

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
		| Some r -> Some (result_to_html r)
		| None -> None
