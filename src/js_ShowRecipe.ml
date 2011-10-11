
open Js
open Js_primitives
open Js_common

let edit_div = Js_common.EID.div Common.Recipe.edit_div_id
let container_div = Js_common.EID.div Common.Recipe.container_div_id

type t = {
	textarea : Dom_html.textAreaElement Js.t;
	edit_div : Dom_html.divElement Js.t;
	submit : Dom_html.buttonElement Js.t;
}

let save_text = "Сохранить"

let saved_text = "Сохранено"

let saving_text = "Сохрянется…"

let error_text = "Текст содержит ошибки"

let save t =
	setText t.submit saving_text;
	let text = to_string (t.textarea##value) in
	lwt recipe_name = Js_mlvar.RecipeName.get () in
	lwt r = Js_API.request ~args:["q", recipe_name; "text", text] Common.API.path_recipe_set in
	match r with
		| API.Action.Ok ->
			setText t.submit saved_text;
			t.submit##disabled <- _true;
			Lwt.return ()
		| API.Action.Error error ->
			alert ("Произошла ошибка сохранения рецепта:\n\n" ^ error);
			setText t.submit save_text;
			Lwt.return ()

let save_pressed t _ =
	save t

let update_preview text =
	let html = Fr_parse.to_html text in
	match html with
		| None ->
			Lwt.return false
		| Some html ->
			lwt container_div = container_div () in
			removeChilds container_div;
			lwt _ = Js_HTML.to_JS container_div html in
			Lwt.return true

let edit_keypressed =
	let is_active = ref false in
	let prev_text = ref None in
	fun t _ ->
		let text = to_string (t.textarea##value) in
		if !prev_text = None then
			prev_text := Some text
		else ();
		match !prev_text with
			| None -> Lwt.return ()
			| Some prev ->
				if text <> prev then
					if not !is_active then
					begin
						t.submit##disabled <- _true;
						setText t.submit save_text;
						is_active := true;
						lwt _ = Lwt_js.sleep 0.5 in
						lwt result = update_preview text in
						if result then
							t.submit##disabled <- _false
						else
							setText t.submit error_text;
						prev_text := Some text;
						is_active := false;
						Lwt.return ()
					end
					else
						Lwt.return ()
				else
					Lwt.return ()

let show_edit t =
	lwt edit_div = edit_div () in
	lwt recipe_name = Js_mlvar.RecipeName.get () in
	lwt r = Js_API.request ~args:["q", recipe_name] Common.API.path_recipe_get in
	t.textarea##value <- string r.API.Recipe.text;
	edit_div##style##display <- string Css_main.block;
	Lwt.return ()

let hide_edit t =
	lwt edit_div = edit_div () in
	edit_div##style##display <- string Css_main.none;
	Lwt.return ()

let init_edit () =
	lwt edit_div = edit_div () in
	let textarea = Dom_html.createTextarea doc in
	let submit = Dom_html.createButton doc in
	appendText submit save_text;
	submit##disabled <- _true;
	let t = {
		textarea = textarea;
		edit_div = edit_div;
		submit = submit;
	} in
	textarea##onkeyup <- handler (edit_keypressed t) _true;
	submit##onclick <- handler (save_pressed t) _true;
	textarea##className <- string Css_main.Recipe.edit_text_textarea;
	Dom.appendChild edit_div textarea;
	Dom.appendChild edit_div submit;
	Lwt.return t

let init () =
	lwt t = init_edit () in
	Js_login.add_on_login (fun _ -> eignore show_edit t);
	Js_login.add_on_logout (fun _ -> eignore hide_edit t);
	Lwt.return ()
