
open Js
open Js_primitives

let vname = "cookie"

module Expires = struct
	type t =
		| Never
		| Seconds of float
		| OnClose

	let tag = "; expires="

	let to_string = function
		| Never -> tag ^ "Fri, 2038-01-01 00:00:00 GMT"
		| Seconds o ->
			let t = Js_date.now () in
			let t = t +. o in
			let now = jsnew date_fromTimeValue (t *. 1000.) in
			let r = now##toGMTString () in
			tag ^ (to_string r)
		| OnClose -> ""
end

let set ?(expires=Expires.OnClose) name value =
	let expires = Expires.to_string expires in
	let v = name ^ "=" ^ (to_string (escape (string value))) ^ expires in
	Unsafe.set doc vname v

let all =
	let sep = Regexp.regexp ";" in
	let eq = Regexp.regexp "([^=]+)=(.+)" in
	fun () ->
		let v = Unsafe.get doc vname in
		let v = to_string v in
		match v with
			| "" -> []
			| v ->
				let l = Regexp.split sep v in
				mapl l (
					let r = Regexp.string_match eq __ 0 in
					match r with
						| Some r ->
							let n = match Regexp.matched_group r 1 with Some v -> v | None -> raise Not_found in
							let v = match Regexp.matched_group r 2 with Some v -> v | None -> raise Not_found in
							n, v
						| None -> raise Not_found
		)

let get name =
	try
		let l = all () in
		let r = List.assoc name l in
		Lwt.return r
	with
		| e -> Lwt.fail e

let exists name =
	let l = all () in
	List.mem_assoc name l

let remove name =
	set ~expires:(Expires.Seconds (-99999999.)) name ""
