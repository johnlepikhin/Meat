
open Js

module DH = Dom_html

module CS = Css_main.Search.Results

module ShowIngridients = struct
	module F = struct
		let name = Common.Results.show_ingridients_f

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
