
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

module Seed = struct
	type t = string
end

module Login = struct
	type t =
		| Ok of string
		| Error
end

type 'a t =
	| Error of string
	| Data of 'a

let to_string v =
	let s = Marshal.to_string v [] in
	let l = String.length s in
	let r = Buffer.create (l*3) in
	for i=0 to l-1 do
		let c = s.[i] in
		let code = Char.code c in
		if c = '\\' || code < 32 || code > 127 then
			Buffer.add_string r (Printf.sprintf "\\%02x" code)
		else
			Buffer.add_char r c
	done;
	Buffer.contents r

let of_string s =
	let l = String.length s in
	let b = Buffer.create l in
	let rec loop pos =
		if pos >= l then
			()
		else
			match s.[pos] with
				| '\\' ->
					let h = String.sub s (pos+1) 2 in
					let h = "0x" ^ h in
					let c = int_of_string h in
					let c = Char.chr c in
					Buffer.add_char b c;
					loop (pos+3)
				| c ->
					Buffer.add_char b c;
					loop (pos+1)
	in
	try
		loop 0;
		let s = Buffer.contents b in
		let r = Marshal.from_string s 0 in
		Some r
	with
		| _ -> None
