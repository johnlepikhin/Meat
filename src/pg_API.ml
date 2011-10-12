
module R = T_processor.Common

let return v =
	let r = API.to_string v in
	Lwt.return r

let ok v =
	return (API.Data v)

let error v =
	return (API.Error v)

let internal_error () = error "Internal error"

let no_post req = error "You must POST data"

let wrong_parameters sp _ = error "Wrong parameters"

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

let recipe_get req =
	let name = req.R.post in
	lwt r = PGSQL(req.R.db) "select recipe.name, recipe.text
		from recipe
		where recipe.name=$name"
	in
	match r with
		| [(name, text)] ->
			let r = {
				API.Recipe.title = name;
				API.Recipe.text = text;
			} in
			ok r
		| _ ->
			internal_error ()

let recipe_set req =
	let (name, text) = req.R.post in
	let recipe = Fr_parse.parse text in
	match recipe with
		| None -> Processor.PAPI.fail "Описание рецепта содержит ошибки!"
		| Some recipe ->
			lwt r = PGSQL(req.R.db) "select id from recipe where name=$name" in
			match r with
				| [(id)] ->
					let ingridient_count = Int32.of_int (List.length recipe.T_recipe.ingridients) in
					lwt _ = PGSQL(req.R.db) "delete from ingridient where orig_id=$id" in
					lwt _ = Lwt_list.iter_s (fun i ->
						let name = i.T_recipe.Ingridient.name in
						lwt ing_id = PGSQL(req.R.db) "select id from recipe where name=$name" in
						match ing_id with
							| [(ing_id)] -> PGSQL(req.R.db) "insert into ingridient (orig_id, ref_id) values ($id, $ing_id)"
							| _ -> Processor.PAPI.fail ("Ингридиент с именем '" ^ name ^ "' не найден")
					) recipe.T_recipe.ingridients in
					lwt _ = PGSQL(req.R.db) "update recipe set text=$text, ingridient_count=$ingridient_count where name=$name" in
					ok ()
				| _ ->
					Processor.PAPI.fail "Рецепт с таким именем не найден"

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
	match row with
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
			ok info
		| _ ->
			lwt _ = Lwt_unix.sleep (Random.float 3.) in
			error "Неправильное имя пользователя или пароль)"

let logout req =
	lwt _ = Eliom_sessions.remove_persistent_session_data ~sp:req.R.sp ~table:Session.User.user () in
	req.R.userinfo <- Lazy.lazy_from_fun (fun () -> Lwt.return None);
	ok ()

let userinfo req =
	lwt ui = Processor.PAPI.userinfo req in
	let ui = match ui with
		| Some u -> Some u.Session.User.info
		| None -> None
	in
	ok ui
