
let version = 1

module Page = struct
	type t =
		| Main
		| SearchResults
		| ShowRecipe
end

module SearchForm = struct
	type autocomplete_list = string list
end

module SearchResults = struct
	type ingridients_list = string list

	type ids_list = int list
end

module RecipeNameComplete = struct
	type t = string list

	let path = ["recipe"; "name"; "complete"]
end

module RecipeIngridients = struct
	type t = string list

	let path = ["recipe"; "ingridients"]
end

module Recipe = struct
	type name = string

	type t = {
		title : string;
		text : string;
	}
end

module RecipeSet = struct
	type t = unit

	let path = ["recipe"; "set"]
end

module RecipeGet = struct
	type t = Recipe.t

	let path = ["recipe"; "get"]
end

module Seed = struct
	type t = string

	let path = ["seed"]
end

module Login = struct
	type t = {
		username : string;
		first_name : string;
		last_name : string option;
		person : string;
	}

	let path = ["login"]
end

module Logout = struct
	type t = unit

	let path = ["logout"]
end

module UserInfo = struct
	type t = Login.t option

	let path = ["userinfo"]
end

type 'a t =
	| Error of string
	| Data of 'a

let to_string = Bit6.a_to_bit6

let of_string = Bit6.bit6_to_a
