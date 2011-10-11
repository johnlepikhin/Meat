%{
	open Lexing
%}

%token EOF QLeftBrace QRightBrace QLeftFigure QRightFigure Eq
%token Step Component Recipe
%token <string> Text Lident Quoted

%start input
%type <T_recipe.t> input

%%

input:
	| Component Quoted {
		T_recipe.Component {
			T_recipe.Component.name = $2;
		}
	}
	| Recipe Quoted QLeftFigure steps QRightFigure {
		T_recipe.Recipe {
			T_recipe.Recipe.name = $2;
			T_recipe.Recipe.steps = $4;
		}
	}

steps:
	| { [] }
	| Step step_args Text steps {
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
