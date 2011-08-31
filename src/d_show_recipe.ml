open XHTML.M

module C = Css_main
module CR = C.Recipe

let f name count =
	let count = Int32.to_string count in
	let r = Page_template.main ~js:"/js/js_recipe.js" <<
		<div class=$Css_main.Main.Content.container_div$>
			$str:name$ содержит $str:count$ ингридиентов.
		</div>
	>> in
	Lwt.return r

let not_found name =
	let r = Page_template.main ~js:"/js/js_recipe.js" <<
		<div class=$Css_main.Main.Content.container_div$>
			Рецепт $str:name$ у нас еще не описан.
		</div>
	>> in
	Lwt.return r
