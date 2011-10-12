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
	module A = API

	let no_post_params path = new_service
		~path
		~get_params:unit
		()

	let api path params =
		let path = "api" :: path in
		let fallback = no_post_params path in
		let service = new_post_service ~fallback ~post_params:params () in
		fallback, service

	let recipe_name_complete = api A.RecipeNameComplete.path (string "q")
	let recipe_ingridients = api A.RecipeIngridients.path (string "q")
	let recipe_get = api A.RecipeGet.path (string "q")
	let recipe_set = api A.RecipeSet.path (string "q" ** string "text")

	let login = api A.Login.path (string "username" ** string "hash")
	let logout = api A.Logout.path unit
	let seed = api A.Seed.path unit
	let userinfo = api A.UserInfo.path unit
end
