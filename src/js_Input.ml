
open Js_primitives

module DH = Dom_html

module M = struct
	type t = {
		input : DH.inputElement Js.t;
		container : DH.divElement Js.t;
	}

	type style = {
		s_input : string;
		s_container : string;
	}

	let init ~style initial_value =
		let input = DH.createInput doc in
		let container = DH.createDiv doc in
		Dom.appendChild container input;
		container##className <- Js.string style.s_container;
		input##className <- Js.string style.s_input;
		input##value <- initial_value;
		let r = {
			input = input;
			container = container;
		} in
		r

	let get_value t = t.input##value

	let set_focus t = t.input##focus ()

	let get_container t =
		Lwt.return t.container

	let update_view div t =
		let content = doc##createTextNode (t.input##value) in
		Js_common.removeChilds div;
		Dom.appendChild div content

end

module Form = Js_EditArea.F(M)(Js_controllers.LoggedIn)(Js_Form.Param)
