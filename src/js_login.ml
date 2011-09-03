open Js

module Login = struct
	let f name =
		let make_msg = function
			| Microml.String _ -> None
			| Microml.List lst ->
				let rec loop = function
					| Microml.String s :: tl -> s :: loop tl
					| Microml.List _ :: tl -> loop tl
					| [] -> []
				in
				let l = loop lst in
				let r = String.concat "\n" l in
				Some r
		in
		let call name =
			lwt r = Js_common.request ~get_args:["q", (to_string name)] Common.API.path_recipe_ingridients in
			let msg = make_msg r in
			match msg with
				| None -> Lwt.return ()
				| Some msg -> Js_UI.Hint.show msg
		in
		Js_common.wrap_error call name
end

