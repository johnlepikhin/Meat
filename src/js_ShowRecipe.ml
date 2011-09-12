
open Js_primitives
open Js_common

let edit_div = Js_common.EID.div Common.Recipe.edit_div_id
let container_div = Js_common.EID.div Common.Recipe.container_div_id

let update_preview text =
	let html = Fr_parse.to_html text in
	match html with
		| None ->
			Lwt.return ()
		| Some html ->
			lwt container_div = container_div () in
			removeChilds container_div;
			let rec loop = function
				| [] -> Lwt.return ()
				| hd :: tl ->
					lwt _ = Js_HTML.to_JS container_div hd in
					loop tl
			in
			loop html

let edit_keypressed textarea _ =
	let text = Js.to_string (textarea##value) in
	update_preview text

let show_edit () =
	lwt edit_div = edit_div () in
	let textarea = Dom_html.createTextarea doc in
	textarea##onkeyup <- handler (edit_keypressed textarea) Js._true;
	Dom.appendChild edit_div textarea;
	Lwt.return ()

let hide_edit () =
	lwt edit_div = edit_div () in
	removeChilds edit_div;
	Lwt.return ()

let init () =
	show_edit ()
