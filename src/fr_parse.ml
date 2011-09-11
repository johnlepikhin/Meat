
open L_HTML

let rec loop_steps f = function
	| [] -> ()
	| hd :: tl ->
		f hd;
		loop_steps f tl

let rec to_html = function
	| [] -> []
	| step :: tl -> div [text step.T_recipe.Step.text] :: to_html tl

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

let to_html =
	try
		let l = Lexing.from_string s in
		let steps = Fr_grammar.input Fr_lexer.recipe l in
		let r = to_html steps in
		Some r
	with
		| _ -> None
