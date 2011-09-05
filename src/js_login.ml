open Js
open Js_primitives
open Js_common
open Js_UI

module DH = Dom_html
module C = Common

let login_div = EID.init C.Login.login_container_id DH.CoerceTo.div
let logout_div = EID.init C.Login.logout_container_id DH.CoerceTo.div
let logout_button_div = EID.init C.Login.logout_id DH.CoerceTo.div
let username_input = EID.init C.Login.username_input_id DH.CoerceTo.input
let password_input = EID.init C.Login.password_input_id DH.CoerceTo.input
let username_submit = EID.init C.Login.username_submit_id DH.CoerceTo.input
let username_div = EID.init C.Login.username_div_id DH.CoerceTo.div

let show_login () =
	lwt login_div = login_div () in
	login_div##style##display <- string "block";
	lwt logout_div = logout_div () in

	logout_div##style##display <- string "none";
	Lwt.return ()

let show_logout uname =
	lwt login_div = login_div () in
	login_div##style##display <- string "none";

	lwt logout_div = logout_div () in
	lwt username_div = username_div () in
	logout_div##style##display <- string "block";
	username_div##innerHTML <- string uname;
	Lwt.return ()

let do_login user password =
	lwt seed = Js_API.request ~args:[] C.API.path_seed in
	let hash = Js_password.encrypt_plain ~seed password in
	let args = [
		"username", user;
		"hash", hash;
	] in
	lwt r = Js_API.request ~args C.API.path_login in
	match r with
		| API.Login.Ok username ->
			show_logout username
		| API.Login.Error -> 
			alert "Не правильное имя пользователя или пароль.";
			Lwt.return ()

let processing_login = ref false

let enable () =
	processing_login := false;
	lwt username = username_input () in
	lwt password = password_input () in
	lwt submit = username_submit () in
	username##disabled <- Js._false;
	password##disabled <- Js._false;
	submit##disabled <- Js._false;
	Lwt.return ()

let disable () =
	processing_login := true;
	lwt username = username_input () in
	lwt password = password_input () in
	lwt submit = username_submit () in
	username##disabled <- Js._true;
	password##disabled <- Js._true;
	submit##disabled <- Js._true;
	Lwt.return ()

let login_pressed _ =
	if not !processing_login then
	begin
		lwt _ = disable () in
		lwt username = EID.string_of_field username_input in
		lwt password = EID.string_of_field password_input in
		lwt p = password_input () in
		p##value <- string "";
		try_lwt
			lwt _ = do_login username password in
			enable ()
		with
		| e ->
			lwt _ = enable () in
			Lwt.fail e
	end
	else
		Lwt.return ()
		

let do_logout _ =
	lwt r = Js_API.request ~args:[] C.API.path_logout in
	match r with
		| API.Action.Ok -> show_login ()
		| API.Action.Error s ->
			alert s;
			Lwt.return ()

let do_kb_login e =
	if e##keyCode = 13 then
		login_pressed ()
	else
		Lwt.return ()

let init () =
	let username_var = Js_fun.var C.Login.username_var in
	lwt username_submit = username_submit () in
	lwt username_input = username_input () in
	lwt password_input = password_input () in
	username_submit##onmouseup <- handler login_pressed _true;
	username_input##onkeypress <- handler do_kb_login _true;
	password_input##onkeypress <- handler do_kb_login _true;
	lwt logout_button_div = logout_button_div () in
	logout_button_div##onmouseup <- handler do_logout _true;
	match username_var with
		| None -> show_login ()
		| Some username -> show_logout username
