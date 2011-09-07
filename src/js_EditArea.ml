open Js_primitives

module DH = Dom_html

module type EDITWIDGET = sig
	type t

	val init: DH.divElement Js.t -> Js.js_string Js.t -> t

	val get_value: t -> Js.js_string Js.t

	val set_focus: t -> unit

	val get_container: t -> DH.divElement Js.t Lwt.t

	val update_view: DH.divElement Js.t -> t -> unit
end

module type CONTROLLER = sig
	type t

	val can_edit: t -> bool Lwt.t

	val set_value: t -> Js.js_string Js.t -> unit

	val get_value: t -> Js.js_string Js.t
end

module F = functor(E : EDITWIDGET) -> functor(C : CONTROLLER) -> struct
	type mode =
		| Edit
		| View

	type t = {
		div : DH.divElement Js.t;
		edit_widget : E.t;
		controller : C.t;
		mutable current_mode : mode;
	}

	let set_edit_mode t =
		match t.current_mode with
			| View ->
				lwt edit = E.get_container t.edit_widget in
				lwt _ = Js_common.replaceElement t.div edit in
				E.set_focus t.edit_widget;
				t.current_mode <- Edit;
				Lwt.return ()
			| Edit ->
				Lwt.return ()

	let set_view_mode t =
		match t.current_mode with
			| Edit ->
				lwt edit = E.get_container t.edit_widget in
				lwt _ = Js_common.replaceElement edit t.div in
				t.current_mode <- View;
				E.update_view t.div t.edit_widget;
				let newval = E.get_value t.edit_widget in
				C.set_value t.controller newval;
				Lwt.return ()
			| View ->
				Lwt.return ()

	let onmouseover t _ = 
		lwt can_edit = C.can_edit t.controller in
		if can_edit then
			set_edit_mode t
		else
			Lwt.return ()

	let onmouseout t _ =
		set_view_mode t

	let make ~controller id =
		lwt div = (Js_common.EID.init id Dom_html.CoerceTo.div) () in
		let initial_value = C.get_value controller in
		let edit_widget = E.init div initial_value in
		let r = {
			div = div;
			edit_widget = edit_widget;
			controller = controller;
			current_mode = View;
		} in
		div##onmouseover <- Js_common.handler (onmouseover r) Js._true;
		lwt edit_div = E.get_container edit_widget in
		edit_div##onmouseout <- Js_common.handler (onmouseout r) Js._true;
		E.update_view div edit_widget;
		Lwt.return r
end


