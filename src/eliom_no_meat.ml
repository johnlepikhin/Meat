
open Eliom_predefmod

let xhtml ~service ~page_type f =
	Xhtmlcompact.register ~service (Processor.Page.call ~f ~page_type)

let main = xhtml ~service:Services.main ~page_type:API.Page.Main Pg_main.f

let search_results = xhtml ~service:Services.search_results ~page_type:API.Page.SearchResults Pg_search_results.f

let show_recipe = xhtml ~service:Services.show_recipe ~page_type:API.Page.ShowRecipe Pg_show_recipe.f

module API = struct
	module S = Services.API
	module P = Pg_API

	let content_type = "text/plain"

	let make service f =
		let f sp get post =
			lwt r = Processor.Common.call ~f sp () post in
			Lwt.return (r, content_type)
		in
		let error_handler sp _ =
			lwt r = Pg_API.wrong_parameters sp () in
			Lwt.return (r, content_type)
		in
		Text.register
			~service
			~error_handler
			~charset:"x-user-defined"
			f

	let register (service_no_post, service) f =
		make service_no_post Pg_API.no_post;
		make service f

	let _ = register S.recipe_name_complete P.recipe_name_complete
	let _ = register S.recipe_ingridients P.recipe_ingridients
	let _ = register S.recipe_get P.recipe_get
	let _ = register S.login P.login
	let _ = register S.logout P.logout
	let _ = register S.seed P.seed
	let _ = register S.userinfo P.userinfo
end
