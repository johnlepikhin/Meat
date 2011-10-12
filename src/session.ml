
let seed : string Eliom_sessions.volatile_table = Eliom_sessions.create_volatile_table ()

module User = struct
	type t = {
		id : int32;
		info : API.Login.t;
	}

	let user : t Eliom_sessions.persistent_table = Eliom_sessions.create_persistent_table "user"
end
