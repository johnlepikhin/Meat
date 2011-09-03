
open Css
open Selector

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
			"width", "100%";
			"height", "100%";
		]

		let container_div = make (Id int_id) [
			"border", "10px solid #d0d0d0";
		]

		let table = make (Id int_id) [
			"width", "100%";
		]

		let td_ingridients = make (Id int_id) [
			"width", "30%";
		]

		module Ingridients = struct
			let input = make (Id int_id) [
				"border", "1px solid #d0d0d0";
				"font-size", "1.5em";
				"width", "100%";
			]

			module Autocomplete = struct
				let container_div_empty = make (Id int_id) [
					"border", "1px solid #d0d0d0";
					"display", "none";
				]

				let container_div_full = make (Id int_id) [
					"border", "1px solid #d0d0d0";
					"display", "block";
					"width", "100%";
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
				"border", "1px solid #d0d0d0";
				"display", "none";
			]

			let container_div_full = make (Id int_id) [
				"border", "1px solid #d0d0d0";
				"display", "block";
			]

			let element_div = make (Id int_id) [
				"border", "1px solid green";
			]
	
			let delete_div = make (Id int_id) [
				"border", "1px solid red";
				"float", "left";
				"width", "20px";
				"cursor", "pointer";
			]
	
			let content_div = make (Id int_id) [
				"border", "1px solid green";
			]
		end
	end

	module Results = struct
		let container_div = make (Id int_id) [
			"border", "1px solid #d0d0d0";
			"margin-top", "30px";
			"margin-bottom", "30px";
		]

		let element_div = make (Id int_id) [
			"border", "1px solid #d0d0d0";
		]

		let name_div = make (Id int_id) [
			"float", "left";
			"width", "30%";
		]

		let info_div = make (Id int_id) [
		]
	end
end

module Recipe = struct
	let name_div = make (Id int_id) [
		"text-align", "center";
		"font-size", "2em";
	]

	module Ingridients = struct
		let container_ul = make (Id int_id) []
		
		let ingridient_li = make (Id int_id) []
	end
	module Parts = struct
		let container_div = make (Id int_id) []
		
		let part_div = make (Id int_id) [
			"padding", "10px";
		]
		
		let part_number = make (Id int_id) [
			"display", "inline";
			"font-size", "1.5em";
			"padding", "10px";
		]
		
	end
end
