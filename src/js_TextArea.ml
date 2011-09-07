
open Js_primitives

module DH = Dom_html

module M = struct
	type t = {
		textarea : DH.textAreaElement Js.t;
		container : DH.divElement Js.t;
	}

	let init div initial_value =
		let textarea = DH.createTextarea doc in
		let container = DH.createDiv doc in
		Dom.appendChild container textarea;
		container##className <- div##className;
		textarea##className <- div##className;
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

module C = struct
	type t = string ref

	let can_edit t =
		Js_login.is_authenticated ()

	let set_value t v =
		t := Js.to_string v

	let get_value t =
		let v = !t in
		Js.string v
end

include Js_EditArea.F(M)(C)
