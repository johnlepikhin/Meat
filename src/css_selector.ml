
type t =
	| Id of int

let to_string = function
	| Id i -> "i" ^ (string_of_int i)

let id = Id (Id.get ())
