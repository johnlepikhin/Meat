
module Property = struct
	type t = string * string

	let to_string (name, value) = name ^ ":" ^ value
end

module Selector = struct
	type t =
		| Id of int

	let class_name = function
		| Id id -> "c" ^ (string_of_int id)

	let to_string t = "." ^ (class_name t)
end

module Style = struct
	type t = {
		selector : Selector.t;
		properties : Property.t list;
	}

	let to_string t =
		let selector = Selector.to_string t.selector in
		let props = List.map Property.to_string t.properties in
		let props = String.concat ";" props in
		selector ^ "{" ^ props ^ "}"
end

let list = ref []

let register v =
	list := v :: !list;
	Selector.class_name v.Style.selector

let make s properties =
	register {
		Style.selector = s;
		Style.properties = properties;
	}

let to_string () =
	let l = List.rev !list in
	let l = List.map Style.to_string l in
	String.concat "" l
