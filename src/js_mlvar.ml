
module C = Common

module Page = Js_var.GlobalMlVar(struct
	type t = API.Page.t

	let name = C.page_name_var
end)

module UserInfo = Js_var.GlobalMlVar(struct
	type t = API.Login.info option

	let name = C.Login.userinfo_var
end)
