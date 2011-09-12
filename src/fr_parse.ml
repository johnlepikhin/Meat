
open L_HTML

let rec loop_steps f = function
	| [] -> ()
	| hd :: tl ->
		f hd;
		loop_steps f tl

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

let to_html s =
	try
		let l = Lexing.from_string s in
		let steps = Fr_grammar.input Fr_lexer.recipe l in
		let r = to_html 1 steps in
		Some r
	with
		| _ -> None
