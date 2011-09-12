
module Common = struct
	type ('a, 'b) t = {
		sp : Eliom_sessions.server_params;
		get : 'a;
		post : 'b;
		db : Db.t;
		mutable userinfo : Session.User.t option Lwt.t Lazy.t;
	}
end

module Page = struct
	type ('a, 'b) t = {
		common : ('a, 'b) Common.t;
		mutable js_vars : (string * string) list;
		mutable js_scripts : string list option;
		mutable title : string;
		page_type : API.Page.t;
	}
end
