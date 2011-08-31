
open Microml
open Db

let recipe_name_complete sp name () =
	let name = name ^ "%" in
	lwt r = PGSQL(h) "select name
		from recipe
		where name like $name
		limit 10"
	in
	let r = mapl r (String __) in
	Lwt.return (List r)

let recipe_ingridients sp name () =
	lwt r = PGSQL(h) "select recipe.name
		from ingridient
		left join recipe on recipe.id=ingridient.ref_id
		where ingridient.orig_id=(select id from recipe where name=$name)"
	in
	let r = mapl r (String __) in
	Lwt.return (List r)
