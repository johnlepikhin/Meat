
open Js

module ShowIngridients = struct
	module F = struct
		let name = Common.Results.show_ingridients_f

		type input = Js.js_string Js.t

		type returns = unit Lwt.t

		let make_args msg = [| Js.Unsafe.inject msg |]

		let f name =
			let make_msg lst =
				let r = String.concat "\n" lst in
				Some r
			in

			let call name =
				lwt r = Js_API.request_list ~get_args:["q", (to_string name)] Common.API.path_recipe_ingridients in
				let msg = make_msg r in
				match msg with
					| None -> Lwt.return ()
					| Some msg -> Js_UI.Hint.show msg
			in
			Js_common.wrap_error call name
	end

	module M = Js_fun.F(F)

	let show = M.call
end
