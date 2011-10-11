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
	| Component Quoted EOF {
		T_recipe.Component {
			T_recipe.Component.name = $2;
		}
	}
	| Recipe Quoted QLeftFigure recipe_entries QRightFigure EOF {
		let steps = ref [] in
		let ingridients = ref [] in
		List.map (function
			| Fr_type.Step v -> steps := v :: !steps
			| Fr_type.Ingridient v -> ingridients := v :: !ingridients
		) $4;
		T_recipe.Recipe {
			T_recipe.Recipe.name = $2;
			T_recipe.Recipe.steps = !steps;
			T_recipe.Recipe.ingridients = !ingridients;
		}
	}

recipe_entries:
	| { [] }
	| Step step_args Text recipe_entries {
		let s = T_recipe.Step.make $3 in
		Fr_type.Step s :: $4
	}
	| Component Quoted recipe_entries {
		let s = T_recipe.Ingridient.make $2 in
		Fr_type.Ingridient s :: $3
	}


step_args:
	| { [] }
	| QLeftBrace param_list QRightBrace { $2 }

param_list:
	| { [] }
	| Lident Eq value param_list { ($1, $3) :: $4 }

value:
	| Lident { $1 }
