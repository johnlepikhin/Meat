
open Js

module ShowIngridients = struct
	module F = struct
		let name = Common.Results.show_ingridients_f

		type input = Js.js_string Js.t

		type returns = unit Lwt.t

		type real_returns = unit Lwt.t

		let make_args msg = [| Js.Unsafe.inject msg |]

		let make_ret _ = Lwt.return ()

		let f name =
			let call name =
				lwt r = Js_API.request ~args:["q", (to_string name)] Common.API.path_recipe_ingridients in
				let msg = String.concat "\n" r in
				Js_UI.Hint.show msg
			in
			Js_common.wrap_error call name
	end

	module M = Js_fun.NewFUN(F)

	let show = M.call
end
