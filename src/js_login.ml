open Js
open Js_primitives
open Js_common
open Js_UI

module DH = Dom_html
module C = Common

let on_login = ref []
let on_logout = ref []

let login_div = EID.init C.Login.login_container_id DH.CoerceTo.div
let logout_div = EID.init C.Login.logout_container_id DH.CoerceTo.div
let logout_button_div = EID.init C.Login.logout_id DH.CoerceTo.div
let username_input = EID.init C.Login.username_input_id DH.CoerceTo.input
let password_input = EID.init C.Login.password_input_id DH.CoerceTo.input
let username_submit = EID.init C.Login.username_submit_id DH.CoerceTo.input
let username_div = EID.init C.Login.username_div_id DH.CoerceTo.div

let cookie_name = "eliompersistentsession|"

let have_cookie () =
	Js_cookie.exists cookie_name

let get_userinfo () =
	if have_cookie () then
		lwt info = Js_mlvar.UserInfo.get_opt () in
		match info with
			| Some info -> Lwt.return info
			| None ->
				lwt info = Js_API.request ~args:[] Common.API.path_userinfo in
				Js_mlvar.UserInfo.set info;
				Lwt.return info
	else
		Lwt.return None

let logged_out () =
	lwt login_div = login_div () in
	login_div##style##display <- string Css_main.block;
	lwt logout_div = logout_div () in

	logout_div##style##display <- string Css_main.none;
	Js_mlvar.UserInfo.set None;
	List.iter (fun f -> f ()) !on_logout;
	Lwt.return ()

let logged_in info =
	lwt login_div = login_div () in
	login_div##style##display <- string Css_main.none;

	lwt logout_div = logout_div () in
	lwt username_div = username_div () in
	logout_div##style##display <- string Css_main.block;
	username_div##innerHTML <- string info.API.Login.person;
	Js_mlvar.UserInfo.set (Some info);
	List.iter (fun f -> f info) !on_login;
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
		| API.Login.Ok info ->
			logged_in info
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
		| API.Action.Ok ->
			logged_out ()
		| API.Action.Error s ->
			alert s;
			Lwt.return ()

let do_kb_login e =
	if e##keyCode = 13 then
		login_pressed ()
	else
		Lwt.return ()

let is_authenticated () =
	lwt info = get_userinfo () in
	match info with
		| None -> Lwt.return false
		| Some _ -> Lwt.return true

let add_on_login f =
	on_login := f :: !on_login

let add_on_logout f =
	on_logout := f :: !on_logout

let if_authenticated f =
	lwt have_auth = is_authenticated () in
	if have_auth then
		f ()
	else
		Lwt.return ()

let init () =
	lwt username_submit = username_submit () in
	lwt username_input = username_input () in
	lwt password_input = password_input () in
	username_submit##onmouseup <- handler login_pressed _true;
	username_input##onkeypress <- handler do_kb_login _true;
	password_input##onkeypress <- handler do_kb_login _true;
	lwt logout_button_div = logout_button_div () in
	logout_button_div##onmouseup <- handler do_logout _true;
	lwt info = get_userinfo () in
	match info with
		| None -> logged_out ()
		| Some info -> logged_in info
