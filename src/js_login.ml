open Js
open Js_primitives
open Js_common
open Js_UI

module DH = Dom_html
module C = Common

let on_login = ref []
let on_logout = ref []

let login_div = EID.div C.Login.login_container_id
let logout_div = EID.div C.Login.logout_container_id
let logout_button_div = EID.div C.Login.logout_id
let username_input = EID.init C.Login.username_input_id DH.CoerceTo.input
let password_input = EID.init C.Login.password_input_id DH.CoerceTo.input
let username_submit = EID.init C.Login.username_submit_id DH.CoerceTo.input
let username_div = EID.div C.Login.username_div_id

module SeedC = Js_API.MakeC(API.Seed)
module LoginC = Js_API.MakeC(API.Login)
module LogoutC = Js_API.MakeC(API.Logout)
module UserInfoC = Js_API.MakeC(API.UserInfo)

module UserInfoStorage = Js_storage.MlExpirable(struct type t = API.UserInfo.t let name = "userinfo" end)

let cookie_name = "eliompersistentsession|"

let have_cookie () =
	Js_cookie.exists cookie_name

let set_userinfo = UserInfoStorage.set ~timeout:3600.

let update_userinfo () =
	lwt info = UserInfoC.q [] in
	match info with
		| API.Error s ->
			alert ("Не удалось получить данные пользователя: " ^ s);
			Lwt.return None
		| API.Data info ->
			set_userinfo info;
			Lwt.return info

let get_userinfo () =
	if have_cookie () then
		try_lwt
			lwt v = UserInfoStorage.get () in
			let v = match v with
				| None -> None
				| Some v -> v
			in
			Lwt.return v
		with
			| _ -> update_userinfo ()
	else
		Lwt.return None

let logged_out () =
	lwt login_div = login_div () in
	login_div##style##display <- string Css_main.block;
	lwt logout_div = logout_div () in

	logout_div##style##display <- string Css_main.none;
	set_userinfo None;
	List.iter (fun f -> f ()) !on_logout;
	Lwt.return ()

let logged_in info =
	lwt login_div = login_div () in
	login_div##style##display <- string Css_main.none;

	lwt logout_div = logout_div () in
	lwt username_div = username_div () in
	logout_div##style##display <- string Css_main.block;
	username_div##innerHTML <- string info.API.Login.person;
	set_userinfo (Some info);
	List.iter (fun f -> f info) !on_login;
	Lwt.return ()

let do_login user password =
	lwt seed = SeedC.q [] in
	match seed with
		| API.Error s ->
			alert ("Не удалось войти т.к. не получили seed: " ^ s ^ ". Попробуйте еще раз.");
			Lwt.return ()
		| API.Data seed ->
			let hash = Js_password.encrypt_plain ~seed password in
			let args = [
				"username", user;
				"hash", hash;
			] in
			lwt r = LoginC.q args in
			match r with
				| API.Data info ->
					logged_in info
				| API.Error s ->
					alert s;
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
	lwt r = LogoutC.q [] in
	match r with
		| API.Data _ ->
			logged_out ()
		| API.Error s ->
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
