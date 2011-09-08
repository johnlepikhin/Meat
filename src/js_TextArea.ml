
open Js_primitives

module DH = Dom_html

module M = struct
	type t = {
		textarea : DH.textAreaElement Js.t;
		container : DH.divElement Js.t;
	}

	type style = {
		s_textarea : string;
		s_container : string;
	}

	let init ~style initial_value =
		let textarea = DH.createTextarea doc in
		let container = DH.createDiv doc in
		Dom.appendChild container textarea;
		container##className <- Js.string style.s_container;
		textarea##className <- Js.string style.s_textarea;
		textarea##value <- initial_value;
		let r = {
			textarea = textarea;
			container = container;
		} in
		r

	let get_value t = t.textarea##value

	let set_focus t = t.textarea##focus ()

	let get_container t =
		Lwt.return t.container

	let update_view div t =
		let content = doc##createTextNode (t.textarea##value) in
		Js_common.removeChilds div;
		Dom.appendChild div content

end

module Data = struct
	type t = string ref

	let set_value t v =
		t := Js.to_string v

	let get_value t =
		let v = !t in
		Js.string v
end

include Js_EditArea.F(M)(Js_controllers.LoggedIn)(Data)
