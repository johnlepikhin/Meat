
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
end

module Login = struct
	type t =
		| Ok of string
		| Error of string
end

type 'a t =
	| Data of 'a
	| Error of string
