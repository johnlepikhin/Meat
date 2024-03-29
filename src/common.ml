
let js_prefix = "$$$$"

let body_id = string_id

let page_name_var = js_prefix ^ string_id

module Login = struct
	let login_container_id = string_id

	let logout_container_id = string_id
	let logout_id = string_id

	let username_input_id = string_id
	let password_input_id = string_id
	let username_submit_id = string_id

	let username_div_id = string_id
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

module Recipe = struct
	let path_show = ["recipe"]

	let path_save = ["api"; "recipe"; "save"]

	let param_name = "q"

	let name_var = js_prefix ^ string_id

	let title_id = string_id

	let edit_div_id = string_id

	let container_div_id = string_id
end
