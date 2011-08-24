
let start_year = "2010"

let current_year =
	let time = Unix.time () in
	let time = Unix.gmtime time in
	string_of_int (time.Unix.tm_year + 1900)
