
let js_prefix = "$$$$$$$$"

let body_id = string_id

let page_name_var = js_prefix ^ string_id

module API = struct
	let path_recipe_name_complete = ["api"; "recipe"; "name"; "complete"]
	let path_recipe_ingridients = ["api"; "recipe"; "ingridients"]
	let path_login = ["api"; "login"]
	let path_logout = ["api"; "logout"]
	let path_seed = ["api"; "seed"]
end

module Login = struct
	let login_container_id = string_id

	let logout_container_id = string_id
	let logout_id = string_id

	let username_input_id = string_id
	let password_input_id = string_id
	let username_submit_id = string_id

	let username_div_id = string_id

	let userinfo_var = js_prefix ^ string_id
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

	module Results = struct
(*		let show_ingridients_f = js_prefix ^ string_id *)

		let element_tr_prefix = js_prefix ^ string_id ^ "_"

		let element_delete_div_prefix = js_prefix ^ string_id ^ "_"

		let ids_list_var = js_prefix ^ string_id
	end
end

module Recipe = struct
	let path_show = ["recipe"]

	let param_name = "q"

	let title_id = string_id
end
