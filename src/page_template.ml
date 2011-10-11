
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

module JsVarsBlock = struct
	let block req =
		Processor.Page.add_js_var req Common.page_name_var req.T_processor.Page.page_type;
		let buf = Buffer.create 16300 in
		let rec loop = function
			| [] -> ()
			| (n,v) :: tl ->
				Buffer.add_string buf n;
				Buffer.add_string buf "='";
				Buffer.add_string buf v;
				Buffer.add_string buf "';";
				loop tl
		in
		loop (Processor.Page.js_vars req);
		let script = XHTML.M.cdata_script (Buffer.contents buf) in
		Lwt.return <<
			<script type="text/javascript">$script$</script>
		>>
end

module JsScriptsBlock = struct
	let block req =
		match req.T_processor.Page.js_scripts with
			| None -> [
					<< <script type="text/javascript" src="/js/sha.js"/> >>;
					<< <script type="text/javascript" src="/js/js_main.js"/> >>;
				]
			| Some lst ->
				mapl lst (<< <script type="text/javascript" src=$__$/> >>)
end

let main req content =
	lwt js_vars = JsVarsBlock.block req in
	let js_scripts = JsScriptsBlock.block req in
	Lwt.return <<
		<html>
			<head>
				<title>Просто со вкусом</title>
				<link rel="stylesheet" type="text/css" href="/css/css_main.css"/>
				$list:js_scripts$
			</head>
			<body id=$Common.body_id$>
				$js_vars$
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
