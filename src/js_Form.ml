
module Param = struct
	type t = {
		name : string;
		mutable value : string;
	}

	let get_value t = Js.string t.value

	let set_value t v = t.value <- Js.to_string v

	let make n v = {
		name = n;
		value = v;
	}
end

type t = {
	mutable params : Param.t list;
	path : string list;
}

let init path = {
	params = [];
	path = path;
}

let add t param =
	t.params <- param :: t.params

let submit t =
	let s = String.concat "/" t.path in
	Js_primitives.alert s
