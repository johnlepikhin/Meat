
let get name =
	Db.use (fun db -> PGSQL(db) "select recipe.id, recipe.name, recipe.text, recipe.ingridient_count
		from recipe
		where recipe.name=$name")

let get_ingridients id =
	Db.use (fun db -> PGSQL(db) "select recipe.name
		from ingridient
		left join recipe on recipe.id=ingridient.ref_id
		where ingridient.orig_id=$id")

let f req =
	lwt r = get (Processor.Xhtml.get req) in
	match r with
		| [(id, name, text, ingridients_count)] ->
			lwt ingridients = get_ingridients id in
			Processor.Xhtml.add_js_var req Common.Recipe.name_var name;
			D_show_recipe.f ~ingridients_count ~ingridients ~text req
		| _ ->
			D_show_recipe.not_found req
