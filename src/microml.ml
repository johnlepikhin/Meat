
type t =
	| String of string
	| List of t list

let escape s =
	let rec loop s c r l rs = function
		| n when n<l -> loop s c r l (rs ^ (if s.[n] = c then r else String.make 1 s.[n])) (n+1)
		| _ -> rs
	in
	let s = loop s '\\' "\\\\" (String.length s) "" 0 in
	loop s '"' "\\\"" (String.length s) "" 0

let rec to_string = function
	| String s -> "\"" ^ (escape s) ^ "\""
	| List l -> "[" ^ (String.concat "" (List.map to_string l)) ^ "]"

let rec parse pos s =
	match s.[pos] with
		| '[' ->
			let (lst, newpos) = plist (pos+1) s in
			(List lst, newpos)
		| '"' ->
			let (s, newpos) = pstring (pos+1) s in
			(String s, newpos)
		| _ -> parse (pos+1) s
and plist pos s =
	match s.[pos] with
		| ']' -> ([], pos+1)
		| _ ->
			let (v, newpos) = parse pos s in
			let (tl, newpos) = plist newpos s in
			(v :: tl), newpos
and pstring pos s =
	let ret c posoff =
		let (tl, newpos) = pstring (pos+posoff) s in
		c ^ tl, newpos
	in
	match s.[pos] with
		| '"' -> ("", pos+1)
		| '\\' when s.[pos+1] = '\\' -> ret "\\" 2
		| '\\' when s.[pos+1] = '"' -> ret "\"" 2
		| c -> ret (String.make 1 c) 1

let of_string s =
	let (r, _) = parse 0 s in
	r
