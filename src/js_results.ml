
open Js

module DH = Dom_html

module CS = Css_main.Search.Results

(*
module ShowIngridients = struct
	module F = struct
		let name = Common.Search.Results.show_ingridients_f

		type input = Js.js_string Js.t

		let make_args msg = [| Js.Unsafe.inject msg |]

		let f name =
			let call name =
				lwt r = Js_API.request ~args:["q", (to_string name)] Common.API.path_recipe_ingridients in
				let msg = String.concat "\n" r in
				Js_UI.Hint.show msg
			in
			Js_common.wrap_error call name
	end

	module M = Js_fun.NewACTION(F)

	let show = M.call
end
*)

module LstVar = Js_var.GlobalMlVar(struct
	type t = API.SearchResults.ids_list

	let name = Common.Search.Results.ids_list_var
end)

module RecipesList_F = struct
	let element_tr_prefix = Common.Search.Results.element_tr_prefix

	let delete_div_prefix = Common.Search.Results.element_delete_div_prefix

	let get_list = LstVar.get

	let set_list = LstVar.set

	let can_delete id = Lwt.return true

	let css_element = Css_main.Search.Results.element_tr

	let css_element_hover = Css_main.Search.Results.element_tr_hover

	let try_delete id =
		let r = Js.to_bool (Js_primitives.window##confirm (string "Действительно удалить?")) in
		Lwt.return r
end

module ListEdit = Js_EditTable.EditTable(Js_controllers.LoggedIn)(RecipesList_F)
