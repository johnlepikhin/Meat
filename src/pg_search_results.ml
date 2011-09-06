
let limit = 20L

let search_all ~offset =
	Db.use (fun db -> PGSQL(db) "select recipe.name, recipe.ingridient_count
		from recipe
		limit $limit
		offset $offset")

let search_properties ~properties ~offset =
	let property_length = Int64.of_int (List.length properties) in
	Db.use (fun db -> PGSQL(db) "select recipe.name, recipe.ingridient_count
		from recipe
		left join recipe_property on recipe.id=recipe_property.recipe
		left join property on property.id=recipe_property.property
		where property.name in $@properties
		group by recipe.name, recipe.ingridient_count
		having count(distinct property.id)=$property_length
		order by recipe.name
		limit $limit
		offset $offset")

let search_ingridients ~ingridients ~offset =
	Db.use (fun db -> PGSQL(db) "select recipe.name, recipe.ingridient_count
		from ingridient
		left join recipe on recipe.id=ingridient.orig_id
		left join recipe as ing on ing.id=ingridient.orig_id
		where
			ingridient.ref_id in (select id from recipe where name in $@ingridients)
		group by recipe.name, recipe.ingridient_count
		order by count(*) desc
		limit $limit
		offset $offset")

let search_i_p ~ingridients ~properties ~offset =
	let property_length = Int64.of_int (List.length properties) in
	Db.use (fun db -> PGSQL(db) "select recipe.name, recipe.ingridient_count
		from ingridient
		left join recipe on recipe.id=ingridient.orig_id
		left join recipe_property on recipe.id=recipe_property.recipe
		left join property on property.id=recipe_property.property
		where
			ingridient.ref_id in (select id from recipe where name in $@ingridients)
			and property.name in $@properties
		group by recipe.name, recipe.ingridient_count
		having count(distinct property.id)=$property_length
		order by count(*) desc
		limit $limit
		offset $offset")

let search ~properties ~ingridients =
	match properties, ingridients with
		| [], [] -> search_all
		| _, [] -> search_properties ~properties
		| [], _ -> search_ingridients ~ingridients
		| _, _ -> search_i_p ~ingridients ~properties

let split =
	let rex = Pcre.regexp "\\s*,\\s*" in
	Pcre.split ~rex

let f req =
	let (selected_ingridients, selected_properties) = Processor.Page.get req in
	let search_f = search ~properties:selected_properties ~ingridients:selected_ingridients in
	lwt results = search_f ~offset:0L in
	D_search_results.main req results
