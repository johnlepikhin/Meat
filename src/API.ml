
let version = 1

module Page = struct
	type t =
		| Main
		| SearchResults
		| ShowRecipe
end

module Action = struct
	type t =
		| Ok
		| Error of string
end

module SearchForm = struct
	type autocomplete_list = string list
end

module SearchResults = struct
	type ingridients_list = string list

	type ids_list = int list
end

module ShowRecipe = struct
	type id = string

	type info = {
		title : string;
		parts : (id * string) list;
	}
end

module Seed = struct
	type t = string
end

module Login = struct
	type info = {
		username : string;
		first_name : string;
		last_name : string option;
		person : string;
	}

	type t =
		| Ok of info
		| Error
end

type 'a t =
	| Error of string
	| Data of 'a

let to_string v =
	let s = Marshal.to_string v [] in
	Bit6.encode s

let of_string s =
	try
		let s = Bit6.decode s in
		let r = Marshal.from_string s 0 in
		Some r
	with
		| _ -> None
