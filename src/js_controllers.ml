module type TYPE = sig
	val can_edit: unit -> bool Lwt.t
end

module LoggedIn = struct
	let can_edit t =
		Js_login.is_authenticated ()
end


