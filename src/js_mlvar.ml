
module C = Common

module Page = Js_var.GlobalMlVar(struct
	type t = API.Page.t

	let name = C.page_name_var
end)

module RecipeName = Js_var.GlobalMlVar(struct
	type t = API.Recipe.name

	let name = C.Recipe.name_var
end)

