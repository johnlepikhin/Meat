open Eliom_services
open Eliom_parameters

module C = Common
module CS = C.Search

let main = new_service
	~path:[]
	~get_params:unit
	()

let property_set = set string CS.property

let search_results = new_service
	~path:C.Search.path
	~get_params:((set string CS.ingridient) ** property_set)
	()

let show_recipe = new_service
	~path:C.Recipe.path_show
	~get_params:(suffix (string C.Recipe.param_name))
	()

module API = struct
	let no_post_params path = new_service
		~path
		~get_params:unit
		()

	let no_post_service path post_params =
		let fallback = no_post_params path in
		let service = new_post_service ~fallback ~post_params () in
		fallback, service

	let recipe_name_complete = new_service
		~path:Common.API.path_recipe_name_complete
		~get_params:(string "q")
		()

	let recipe_ingridients = new_service
		~path:Common.API.path_recipe_ingridients
		~get_params:(string "q")
		()

	let (login_no_post, login) = no_post_service Common.API.path_login (string "username")

	let (logout_no_post, logout) = no_post_service Common.API.path_logout unit
end
