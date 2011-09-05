
open Db

let return v =
	let r = API.to_string v in
	Lwt.return r

let ok v =
	return (API.Data v)

let no_post sp () =
	return (API.Error "You must POST data")

let wrong_parameters sp _ =
	return (API.Error "Wrong parameters")

let recipe_name_complete sp name =
	let name = name ^ "%" in
	lwt r = Db.use (fun db -> PGSQL(db) "select name
		from recipe
		where name like $name
		limit 10")
	in
	ok r

let recipe_ingridients sp name =
	lwt r = Db.use (fun db -> PGSQL(db) "select recipe.name
		from ingridient
		left join recipe on recipe.id=ingridient.ref_id
		where ingridient.orig_id=(select id from recipe where name=$name)")
	in
	ok r

let get_seed sp =
	let v = Eliom_sessions.get_volatile_session_data ~sp ~table:Session.seed () in
	match v with
		| Eliom_sessions.Data v ->
			v
		| Eliom_sessions.No_data
		| Eliom_sessions.Data_session_expired ->
			let seed = L_random.bytes 64 in
			Eliom_sessions.set_volatile_session_data ~sp ~table:Session.seed seed;
			seed

let seed sp () =
	let seed = get_seed sp in
	ok seed

let login sp (username, hash) =
	let seed = get_seed sp in
	Eliom_sessions.remove_volatile_session_data ~sp ~table:Session.seed ();
	lwt row = Db.use (fun db -> PGSQL(db) "select id, username, firstname, lastname, password
		from users
		where username=$username")
	in
	lwt r = match row with
		| [id, username, firstname, lastname, password] when hash = Ml_password.encrypt_seed_hash ~seed password ->
			let person = match lastname with
				| Some l -> firstname ^ " " ^ l
				| None -> firstname
			in
			let info = {
				API.Login.username = username;
				API.Login.first_name = firstname;
				API.Login.last_name = lastname;
				API.Login.person = person;
			} in
			let t = {
				Session.User.id = id;
				Session.User.info = info;
			} in
			lwt _ = Eliom_sessions.set_persistent_session_data ~table:Session.User.user ~sp t in
			Lwt.return (API.Login.Ok info)
		| _ ->
			lwt _ = Lwt_unix.sleep (Random.float 3.) in
			Lwt.return API.Login.Error
	in
	ok r

let logout sp () =
	lwt _ = Eliom_sessions.remove_persistent_session_data ~sp ~table:Session.User.user () in
(*
	lwt _ = Eliom_sessions.close_session ~sp () in
*)
	ok API.Action.Ok
