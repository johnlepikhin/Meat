
open Db

let get name =
	PGSQL(h) "select recipe.name, recipe.ingridient_count
		from recipe
		where recipe.name=$name"

let f sp name () =
	lwt r = get name in
	match r with
		| [(name, count)] ->
			D_show_recipe.f name count
		| _ ->
			D_show_recipe.not_found name
