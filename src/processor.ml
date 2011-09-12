
module Common = struct
	open T_processor.Common

	let get_userinfo sp () =
		lwt user = Eliom_sessions.get_persistent_session_data ~table:Session.User.user ~sp () in
		let userinfo = match user with
			| Eliom_sessions.No_data
			| Eliom_sessions.Data_session_expired -> None
			| Eliom_sessions.Data user -> Some user
		in
		Lwt.return userinfo

	let call ~f sp get post =
		Db.use (fun db ->
			lwt _ = PGSQL(db) "begin transaction" in
			let t = {
				sp = sp;
				get = get;
				post = post;
				db = db;
				userinfo = Lazy.lazy_from_fun (get_userinfo sp);
			} in
			try_lwt
				lwt r = f t in
				lwt _ = PGSQL(db) "commit" in
				Lwt.return r
			with
				| e ->
					lwt _ = PGSQL(db) "rollback" in
					Lwt.fail e
		)

	let userinfo t = Lazy.force t.userinfo
end

module Page = struct
	open T_processor.Page
	module C = T_processor.Common

	let call ~f ~page_type sp get post =
		let f common =
			let t = {
				common = common;
				js_vars = [];
				js_scripts = None;
				title = Config.default_title;
				page_type = page_type;
			} in
			f t
		in
		Common.call ~f sp get post

	let userinfo t = Lazy.force t.common.C.userinfo

	let sp t = t.common.C.sp

	let db t = t.common.C.db

	let get t = t.common.C.get

	let post t = t.common.C.post

	let js_vars t = t.js_vars

	let add_js_var t name value =
		t.js_vars <- (name, value) :: t.js_vars
end
