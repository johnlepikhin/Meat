
let init () =
	let form = Js_Form.init ["api"; "test"] in
	let title = Js_Form.Param.make "title" "test" in
	Js_Form.add form title;
	let style = {
		Js_Input.M.s_input = Css_main.Recipe.title_input;
		Js_Input.M.s_container = Css_main.Recipe.title_edit_div;
	} in
	Js_Input.Form.make ~data:title ~style Common.Recipe.title_id
