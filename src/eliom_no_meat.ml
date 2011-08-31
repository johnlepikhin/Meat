
open Eliom_predefmod

let xhtml service f =
	Xhtmlcompact.register ~service f

let main = xhtml Services.main Pg_main.f

let search_results = xhtml Services.search_results Pg_search_results.f

let show_recipe = xhtml Services.show_recipe Pg_show_recipe.f

module API = struct
	open Microml

	let content_type = "text/plain"

	let error_handler =
		let e = String "API error" in
		let r = to_string e in
		let r = r, content_type in
		fun _ _ -> Lwt.return r

	let microml service f =
		let f sp get post =
			lwt mml = f sp get post in
			let content = Microml.to_string mml in
			Lwt.return (content, content_type)
		in
		Text.register
			~error_handler
			~service
			f

	let recipe_name_complete = microml Services.API.recipe_name_complete Pg_API.recipe_name_complete

	let recipe_ingridients = microml Services.API.recipe_ingridients Pg_API.recipe_ingridients
end
