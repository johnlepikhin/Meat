
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

module Recipe = struct
	type info = {
		title : string;
		text : string;
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

let to_string = Bit6.a_to_bit6

let of_string = Bit6.bit6_to_a
