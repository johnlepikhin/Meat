
open Eliom_predefmod

let xhtml service f =
	Xhtmlcompact.register ~service f

let main = xhtml Services.main Pg_main.f

let search_results = xhtml Services.search_results Pg_search_results.f

let show_recipe = xhtml Services.show_recipe Pg_show_recipe.f

module API = struct
	module S = Services.API
	module P = Pg_API

	let content_type = "text/plain"

	let make service f =
		let f sp get post =
			lwt r = f sp post in
			Lwt.return (r, content_type)
		in
		Text.register
			~service
			~charset:"x-user-defined"
			f

	let register (service_no_post, service) f =
		make service_no_post Pg_API.no_post;
		make service f

	let _ = register S.recipe_name_complete P.recipe_name_complete
	let _ = register S.recipe_ingridients P.recipe_ingridients
	let _ = register S.login P.login
	let _ = register S.logout P.logout
end
