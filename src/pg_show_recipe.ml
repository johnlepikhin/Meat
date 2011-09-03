
open Db

let get name =
	PGSQL(h) "select recipe.id, recipe.name, recipe.ingridient_count
		from recipe
		where recipe.name=$name"

let get_ingridients id =
	PGSQL(h) "select recipe.name
		from ingridient
		left join recipe on recipe.id=ingridient.ref_id
		where ingridient.orig_id=$id"

let get_parts id =
	PGSQL(h) "select recipe_parts.text
		from recipe_parts
		where recipe_parts.recipe=$id
		order by recipe_parts.id"

let f sp name () =
	lwt r = get name in
	match r with
		| [(id, name, ingridients_count)] ->
			lwt ingridients = get_ingridients id in
			lwt parts = get_parts id in
			D_show_recipe.f ~ingridients_count ~ingridients ~parts ~sp name
		| _ ->
			D_show_recipe.not_found ~sp name
