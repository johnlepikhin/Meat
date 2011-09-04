
open Db

let encode s =
	let l = String.length s in
	let r = Buffer.create (l*3) in
	for i=0 to l-1 do
		match s.[i] with
			| '\\' as c
			| c when Char.code c < 32 || Char.code c > 127 -> Buffer.add_string r (Printf.sprintf "\\%02x" (Char.code c))
			| c -> Buffer.add_char r c
	done;
	Buffer.contents r

let return v =
	let r = Marshal.to_string v [] in
	let r = encode r in
	Lwt.return r

let ok v =
	return (API.Data v)

let no_post sp () =
	return (API.Error "You must POST data")

let recipe_name_complete sp name =
	let name = name ^ "%" in
	lwt r = PGSQL(h) "select name
		from recipe
		where name like $name
		limit 10"
	in
	ok r

let recipe_ingridients sp name =
	lwt r = PGSQL(h) "select recipe.name
		from ingridient
		left join recipe on recipe.id=ingridient.ref_id
		where ingridient.orig_id=(select id from recipe where name=$name)"
	in
	ok r

let login sp (username) =
	lwt _ = Eliom_sessions.close_session ~sp () in
	lwt _ = Eliom_sessions.set_persistent_session_data ~table:Session.s ~sp username in
	ok (API.Login.Ok username)

let logout sp () =
	lwt _ = Eliom_sessions.close_session ~sp () in
	ok API.Action.Ok
