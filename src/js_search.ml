
open Js
open Js_primitives
open Js_common
open Js_UI

module Import1 = Js_results

module DH = Dom_html
module C = Common

module Ingridients = struct
	module FS = C.Search
	module CF = Css_main.Search.Form

	let input_ingridient = EID.init FS.ingridient_id DH.CoerceTo.input
	let form = EID.init FS.form_id DH.CoerceTo.form

	module Selection = struct
		let ingridients_list = EID.init FS.selected_ingridients_list_id DH.CoerceTo.div
		let hidden_inputs = EID.init FS.selected_ingridients_inputs_id DH.CoerceTo.div

		let list = ref []

		let current () =
			let r = List.filter (fun (n, _) -> n = FS.ingridient) Js_API.URL.args in
			List.map (fun (_, v) -> v) r

		let delete name div input _ =
			removeElement div;
			removeElement input;
			let v = (name, (input, div)) in
			list := List.filter ((<>) v) !list;
			if List.length !list = 0 then
				div##className <- string CF.Selection.container_div_empty
			else ();
			Lwt.return ()

		let select ~name _ =
			if not (List.mem_assoc name !list) then
			begin
				lwt hidden_inputs = hidden_inputs () in
				lwt ingridients_list = ingridients_list () in
				let hidden_input = DH.createInput ~_type:(string "hidden") ~name:(string FS.ingridient) doc in
				hidden_input##value <- string name;
				D.appendChild hidden_inputs hidden_input;
				let element_div = DH.createDiv doc in
				let delete_div = DH.createDiv doc in
				let content_div = DH.createDiv doc in
				element_div##className <- string CF.Selection.element_div;
				content_div##className <- string CF.Selection.content_div;
				delete_div##className <- string CF.Selection.delete_div;
				D.appendChild ingridients_list element_div;
				D.appendChild element_div delete_div;
				D.appendChild element_div content_div;
				appendText content_div name;
				appendText delete_div "X";
		
				delete_div##onclick <- handler (delete name element_div hidden_input) _true;
		
				list := (name, (hidden_input, element_div)) :: !list;

				if List.length !list > 0 then
					ingridients_list##className <- string CF.Selection.container_div_full
				else ();

				lwt input = input_ingridient () in
				input##focus ();
				Lwt.return ()
			end
			else
				Lwt.return ()

		let init () =
			let lst = current () in
			List.iter (fun name -> ignore (select ~name ())) lst;
	end

	module Autocomplete = struct
		module CF = CF.Ingridients.Autocomplete

		let autocomplete_div = EID.init FS.autocomplete_id DH.CoerceTo.div

		let selection = ref None

		let list = ref []

		let clear () =
			lwt ac = autocomplete_div () in
			removeChilds ac;
			KeyEvent.pop [KeyEvent.up];
			KeyEvent.pop [KeyEvent.down];
			ac##className <- string CF.container_div_empty;
			Lwt.return ()

		type move =
			| Up
			| Down
			| Name of string

		let move move _ =
			let div n =
				let (_, div) = List.nth !list n in
				div
			in
			let byname n =
				let rec loop i = function
					| [] -> raise Not_found
					| (name, div) :: _ when n = name -> i
					| _ :: tl -> loop (i+1) tl
				in
				loop 0 !list
			in
			let select n =
				let div = div n in
				div##className <- string CF.active_element_div;
				selection := Some n;
			in
			let unselect n =
				let div = div n in
				div##className <- string CF.inactive_element_div;
			in
			let m = (List.length !list) - 1 in
			begin
				match (!selection, move) with
					| None, Name name ->
						let newpos = byname name in
						select newpos
					| None, _ -> select m
					| Some pos, Up when pos < m ->
						unselect pos;
						select (pos+1)
					| Some pos, Down when pos > 0 ->
						unselect pos;
						select (pos-1)
					| Some pos, Name name ->
						unselect pos;
						let pos = byname name in
						select pos
					| _ -> ()
			end;
			Lwt.return ()

		let select _ =
			match !selection with
				| None -> Lwt.return ()
				| Some pos ->
					let (name, _) = List.nth !list pos in
					Selection.select ~name ()

		let init_keys () =
			KeyEvent.push [KeyEvent.up] (move Up) _false;
			KeyEvent.push [KeyEvent.down] (move Down) _false;
			KeyEvent.push [KeyEvent.enter] select _false

		let add ~ac name =
			let div = DH.createDiv doc in
			div##onclick <- handler (Selection.select ~name) _false;
			div##onmouseover <- handler (move (Name name)) _true;
			D.appendChild ac div;
			appendText div name;
			Some div
	
		let init lst =
			list := [];
			selection := None;
			lwt ac = autocomplete_div () in
			lwt _ = clear () in
			eachl lst (
				let div = add ~ac __ in
				match div with
					| None -> ()
					| Some div -> list := (__, div) :: !list
			);
			init_keys ();
			if List.length lst > 0 then
				ac##className <- string CF.container_div_full
			else ();
			Lwt.return ()
	end

	let submit _ =
		lwt form = form () in
		form##submit ();
		Lwt.return ()

	let onkeyup =
		let last_text = ref "" in
		let is_active = ref false in
		fun el _ ->
			let f () =
				let v = to_string (el##value) in
				if v <> !last_text && String.length v > 0 then
				begin
					lwt lst = Js_API.request_list ~get_args:["q", v] Common.API.path_recipe_name_complete in
					last_text := v;
					Autocomplete.init lst
				end
				else
					Lwt.return ()
			in
			let v = to_string (el##value) in
			if String.length v > 0 then
				if not !is_active then
				begin
					is_active := true;
					lwt _ = Lwt_js.sleep 0.5 in
					begin
						try_lwt
							lwt _ = f () in
							is_active := false;
							Lwt.return ()
						with
							| e ->
								is_active := false;
								Lwt.fail e
					end
				end
				else
					Lwt.return ()
			else
			begin
				last_text := v;
				Autocomplete.clear ()
			end

	let onfocus el _ =
(*		KeyEvent.backup (); *)
		KeyEvent.push [KeyEvent.enter; KeyEvent.ctrl] submit _false;
		Lwt.return ()

	let onblur el _ =
(*		KeyEvent.restore (); *)
(*		Autocomplete.clear () *)
		Lwt.return ()

	let init () =
		lwt el = input_ingridient () in
		el##onkeyup <- handler (onkeyup el) _true;
		el##onfocus <- handler (onfocus el) _true;
		el##onblur <- handler (onblur el) _true;

		Selection.init ();

		KeyEvent.push [KeyEvent.enter; KeyEvent.ctrl] submit _false;
		el##focus ();
		Lwt.return ()

end

module Login = struct
	let login_div = EID.init C.Login.login_container_id DH.CoerceTo.div
	let logout_div = EID.init C.Login.logout_container_id DH.CoerceTo.div
	let logout_button_div = EID.init C.Login.logout_id DH.CoerceTo.div
	let username_input = EID.init C.Login.username_input_id DH.CoerceTo.input
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

	let do_login _ =
		lwt username_input = username_input () in
		let username = to_string (username_input##value) in
		lwt r = Js_API.request_list ~get_args:[] ~post_args:["username", username] C.API.path_login in
		match r with
			| ["username"; s] ->
				show_logout s
			| ["error"; s] ->
				alert s;
				Lwt.return ()
			| _ ->
				alert "Неожиданный ответ";
				Lwt.return ()

	let do_logout _ =
		lwt r = Js_API.request_list ~get_args:[] ~post_args:[] C.API.path_logout in
		match r with
			| ["ok"] ->
				show_login ()
			| ["error"; s] ->
				alert s;
				Lwt.return ()
			| _ ->
				alert "Неожиданный ответ";
				Lwt.return ()

	let init () =
		let username_var = Js_fun.var C.Login.username_var in
		lwt username_submit = username_submit () in
		username_submit##onmouseup <- handler do_login _true;
		lwt logout_button_div = logout_button_div () in
		logout_button_div##onmouseup <- handler do_logout _true;
		match username_var with
			| None -> show_login ()
			| Some username -> show_logout username
end

module PageMain = struct
	let init () =
		ignore (Ingridients.init ())
end

module PageSearchResults = struct
	let init () =
		ignore (Ingridients.init ())
end

module PageShowRecipe = struct
	let init () =
		()
end

let init _ =
	ignore (Js_UI.KeyEvent.init ());
	ignore (Login.init ());
	let page_name = Js_fun.string C.page_name_var in
	let _ = match page_name with
		| Some v when v = C.PageName.main -> PageMain.init ()
		| Some v when v = C.PageName.search_results -> PageSearchResults.init ()
		| Some v when v = C.PageName.show_recipe -> PageShowRecipe.init ()
		| _ -> ()
	in
	Lwt.return ()

let _ = window##onload <- handler init _true
