
module R = T_processor.Common

let return v =
	let r = API.to_string v in
	Lwt.return r

let ok v =
	return (API.Data v)

let no_post req =
	return (API.Error "You must POST data")

let wrong_parameters sp _ =
	return (API.Error "Wrong parameters")

let recipe_name_complete req =
	let name = req.R.post ^ "%" in
	lwt r = PGSQL(req.R.db) "select name
		from recipe
		where name like $name
		limit 10"
	in
	ok r

let recipe_ingridients req =
	let name = req.R.post in
	lwt r = PGSQL(req.R.db) "select recipe.name
		from ingridient
		left join recipe on recipe.id=ingridient.ref_id
		where ingridient.orig_id=(select id from recipe where name=$name)"
	in
	ok r

let get_seed req =
	let v = Eliom_sessions.get_volatile_session_data ~sp:req.R.sp ~table:Session.seed () in
	match v with
		| Eliom_sessions.Data v ->
			v
		| Eliom_sessions.No_data
		| Eliom_sessions.Data_session_expired ->
			let seed = L_random.bytes 64 in
			Eliom_sessions.set_volatile_session_data ~sp:req.R.sp ~table:Session.seed seed;
			seed

let seed req =
	let seed = get_seed req in
	ok seed

let login req =
	let seed = get_seed req in
	Eliom_sessions.remove_volatile_session_data ~sp:req.R.sp ~table:Session.seed ();
	let (username, hash) = req.R.post in
	lwt row = PGSQL(req.R.db) "select id, username, firstname, lastname, password
		from users
		where username=$username"
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
			lwt _ = Eliom_sessions.set_persistent_session_data ~table:Session.User.user ~sp:req.R.sp t in
			req.R.userinfo <- Lazy.lazy_from_fun (fun () -> Lwt.return (Some t));
			Lwt.return (API.Login.Ok info)
		| _ ->
			lwt _ = Lwt_unix.sleep (Random.float 3.) in
			Lwt.return API.Login.Error
	in
	ok r

let logout req =
	lwt _ = Eliom_sessions.remove_persistent_session_data ~sp:req.R.sp ~table:Session.User.user () in
	req.R.userinfo <- Lazy.lazy_from_fun (fun () -> Lwt.return None);
	ok API.Action.Ok

let userinfo req =
	lwt ui = Processor.Common.userinfo req in
	let ui = match ui with
		| Some u -> Some u.Session.User.info
		| None -> None
	in
	ok ui
