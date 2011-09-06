
module LoginBlock = struct
	module Cs = Css_main.Main.Login
	module C = Common.Login

	let block =
		<<
			<div class=$Cs.main_div$>
				<div class=$Cs.login_container_div$ id=$C.login_container_id$>
					<input type="string" name="username" class=$Cs.username_input$ id=$C.username_input_id$/>
					<input type="password" name="password" class=$Cs.password_input$ id=$C.password_input_id$/>
					<input type="button" class=$Cs.username_submit$ id=$C.username_submit_id$/>
				</div>
				<div class=$Cs.logout_container_div$ id=$C.logout_container_id$>
					<div class=$Cs.username_div$ id=$C.username_div_id$/>
					<div class=$Cs.logout_div$ id=$C.logout_id$>выход</div>
				</div>
			</div>
		>>
end

let escape s =
	let l = String.length s in
	let r = Buffer.create (l*3) in
	for i=0 to l-1 do
		match s.[i] with
			| '\\' -> Buffer.add_string r "\\\\"
			| '\'' -> Buffer.add_string r "\\'"
			| c ->
				let cc = Char.code c in
				if cc < 32 || cc > 127 then
					Buffer.add_string r (Printf.sprintf "\\x%02x" cc)
				else
					Buffer.add_char r c
	done;
	Buffer.contents r

let init_js ~sp ~js_vars page_type =
	lwt info = Eliom_sessions.get_persistent_session_data ~table:Session.User.user ~sp () in
	let info = match info with
		| Eliom_sessions.No_data
		| Eliom_sessions.Data_session_expired -> None
		| Eliom_sessions.Data info -> Some info.Session.User.info
	in
	let vars = [
		Common.page_name_var, API.to_string page_type;
		Common.Login.userinfo_var, API.to_string info;
	] @ js_vars in
	let vars = List.map (fun (n,v) -> n ^"='" ^ (escape v) ^ "';") vars in
	let script = String.concat "" vars in
	let script = XHTML.M.cdata_script script in
	Lwt.return <<
		<script type="text/javascript">$script$</script>
	>>

let main ~sp ~js ~page_type ?(js_vars=[]) content =
	lwt init_js = init_js ~sp ~js_vars page_type in
	Lwt.return <<
		<html>
			<head>
				<title>Просто со вкусом</title>
				<script type="text/javascript" src="/js/sha.js"/>
				<script type="text/javascript" src=$js$/>
				<link rel="stylesheet" type="text/css" href="/css/css_main.css"/>
			</head>
			<body id=$Common.body_id$>
				$init_js$
				<div class=$Css_main.Main.Head.div$>
					$LoginBlock.block$
				</div>
				$content$
				<div class=$Css_main.Main.Footer.div$>
					© $str:Config.start_year$–$str:Config.current_year$ $str:Config.copyright$
				</div>
			</body>
		</html>
	>>
