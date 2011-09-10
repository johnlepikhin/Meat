
open Css
open Selector

let block = "block"

let none = "none"

let border width color = "border", ((string_of_int width) ^ "px solid " ^ color)

let width w = "width", ((string_of_int w) ^ "%")
let width100 = width 100

let padding p = "padding", ((string_of_int p) ^ "px")
let padding0 = padding 0

let margin m = "margin", ((string_of_int m) ^ "px")
let margin0 = margin 0

let display d = "display", d
let display_none = display none
let display_block = display block

module Main = struct
	module Head = struct
		let div = make (Id int_id) []
	end

	module Login = struct
		let main_div = make (Id int_id) [
			"background", "#90ff90";
		]

		let login_container_div = make (Id int_id) []

		let username_input = make (Id int_id) []
		let password_input = make (Id int_id) []
		let username_div = make (Id int_id) []
		let username_submit = make (Id int_id) []

		let logout_container_div = make (Id int_id) []

		let logout_div = make (Id int_id) []
	end

	module Footer = struct
		let div = make (Id int_id) [
			"text-align", "center";
			"border-top", "1px solid #d0d0d0";
			"color", "#d0d0d0";
		]
	end

	module Content = struct
		let container_div = make (Id int_id) []
	end
end

module Search = struct
	module Form = struct
		let form_div = make (Id int_id) []
		let submit_button = make (Id int_id) [
			width100;
			"height", "100%";
		]

		let container_div = make (Id int_id) [
			border 10 "#d0d0d0";
		]

		let table = make (Id int_id) [
			width100;
		]

		let td_ingridients = make (Id int_id) [
			width 30;
		]

		module Ingridients = struct
			let input = make (Id int_id) [
				border 1 "#d0d0d0";
				"font-size", "1.5em";
				width100;
			]

			module Autocomplete = struct
				let container_div_empty = make (Id int_id) [
					border 1 "#d0d0d0";
					display_none;
				]

				let container_div_full = make (Id int_id) [
					border 1 "#d0d0d0";
					display_block;
					width100;
				]
	
				let active_element_div = make (Id int_id) [
					"background", "#d0d0ff";
				]
	
				let inactive_element_div = make (Id int_id) [
					"background", "";
				]
			end
		end

		module Properties = struct
			let element_div = make (Id int_id) []
	
			let checkbox = make (Id int_id) []
		end

		module Selection = struct
			let container_div_empty = make (Id int_id) [
				border 1 "#d0d0d0";
				display_none;
			]

			let container_div_full = make (Id int_id) [
				border 1 "#d0d0d0";
				display_block;
			]

			let element_div = make (Id int_id) [
				border 1 "green";
			]
	
			let delete_div = make (Id int_id) [
				border 1 "red";
				"float", "left";
				"width", "20px";
				"cursor", "pointer";
			]
	
			let content_div = make (Id int_id) [
				border 1 "green";
			]
		end
	end

	module Results = struct
		let container_table = make (Id int_id) [
			border 1 "#d0d0d0";
			"margin-top", "30px";
			"margin-bottom", "30px";
			width100;
			"border-collapse", "collapse";
			"border-spacing", "0px";
		]

		let element_tr = make (Id int_id) []

		let element_tr_hover = make (Id int_id) [
			"background", "#9090ff";
		]

		let delete_td = make (Id int_id) [
			padding0;
			"width", "2em";
		]

		let delete_div = make (Id int_id) [
			display_none;
			"cursor", "pointer";
		]

		let name_td = make (Id int_id) [
			padding0;
			width 30;
		]

		let info_td = make (Id int_id) [
			padding0;
		]
	end
end

module Recipe = struct
	let title_div = make (Id int_id) [
		"text-align", "center";
		"font-size", "2em";
		"vertical-align", "middle";
		width100;
		padding 1;
		margin0;
	]

	let title_edit_div = make (Id int_id) [
		"text-align", "center";
		"font-size", "2em";
		"vertical-align", "middle";
		width100;
		padding0;
		margin0;
	]

	let title_input = make (Id int_id) [
		"text-align", "center";
		width 70;
		"font-size", "inherit";
		"font", "inherit";
		"outline", none;
		padding0;
		margin0;
		border 1 "red";
	]

	module Ingridients = struct
		let container_ul = make (Id int_id) []
		
		let ingridient_li = make (Id int_id) []
	end
	module Parts = struct
		let container_div = make (Id int_id) []
		
		let part_div = make (Id int_id) [
			padding 10;
		]
		
		let part_number = make (Id int_id) [
			display "inline";
			"font-size", "1.5em";
			padding 10;
		]
		
	end
end
