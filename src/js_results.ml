
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

module RecipesList = struct
(*
	module OnMouseOver = Js_fun.NewACTION(struct
		let name = Common.Results.onmouseover_f

		type input = DH.divElement Js.t

		let make_args div = [| Js.Unsafe.inject div |]

		let f div = Js_login.if_authenticated (fun _ ->
			div##className <- string CS.elevent_div_hover;
			Lwt.return ()
		)
	end)

	module OnMouseOut = Js_fun.NewACTION(struct
		let name = Common.Results.onmouseout_f

		type input = DH.divElement Js.t

		let make_args div = [| Js.Unsafe.inject div |]

		let f div = Js_login.if_authenticated (fun _ ->
			div##className <- string CS.element_div;
			Lwt.return ()
		)
	end)
*)
end

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

	let can_edit id = Js_login.is_authenticated ()

	let css_element = Css_main.Search.Results.element_tr

	let css_element_hover = Css_main.Search.Results.element_tr_hover

	let try_delete id =
		let r = Js.to_bool (Js_primitives.window##confirm (string "Действительно удалить?")) in
		Lwt.return r
end

module ListEdit = Js_EditTable.EditTable(RecipesList_F)
