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
	module C = Common.API

	let no_post_params path = new_service
		~path
		~get_params:unit
		()

	let api path params =
		let fallback = no_post_params path in
		let service = new_post_service ~fallback ~post_params:params () in
		fallback, service

	let recipe_name_complete = api C.path_recipe_name_complete (string "q")
	let recipe_ingridients = api C.path_recipe_ingridients (string "q")
	let recipe_get = api C.path_recipe_get (string "q")
	let recipe_set = api C.path_recipe_set (string "q" ** string "text")

	let login = api C.path_login (string "username" ** string "hash")
	let logout = api C.path_logout unit
	let seed = api C.path_seed unit
	let userinfo = api C.path_userinfo unit
end
