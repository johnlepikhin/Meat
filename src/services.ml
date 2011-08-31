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
	let recipe_name_complete = new_service
		~path:Common.API.path_recipe_name_complete
		~get_params:(string "q")
		()

	let recipe_ingridients = new_service
		~path:Common.API.path_recipe_ingridients
		~get_params:(string "q")
		()
end
