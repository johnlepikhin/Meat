%{
	open Lexing
%}

%token EOF QLeftBrace QRightBrace Eq
%token Step
%token <string> Text Lident

%start input
%type <T_recipe.Step.t list> input

%%

input:
	| EOF { [] }
	| Step step_args Text input {
		let s = T_recipe.Step.default $3 in
		s :: $4
	}

step_args:
	| { [] }
	| QLeftBrace param_list QRightBrace { $2 }

param_list:
	| { [] }
	| Lident Eq value param_list { ($1, $3) :: $4 }

value:
	| Lident { $1 }
