
let body_id = string_id

module API = struct
	let path_recipe_name_complete = ["api"; "recipe"; "name"; "complete"]
	let path_recipe_ingridients = ["api"; "recipe"; "ingridients"]
end

module Search = struct
	let path = ["search"]

	let property = "property"
	let ingridient = "ingridient"

	let form_id = string_id
	let ingridient_id = string_id
	let autocomplete_id = string_id
	let selected_ingridients_list_id = string_id
	let selected_ingridients_inputs_id = string_id
end

module Results = struct
	let show_ingridients_f = "f" ^ string_id
end

module Recipe = struct
	let path_show = ["recipe"]

	let param_name = "q"
end
