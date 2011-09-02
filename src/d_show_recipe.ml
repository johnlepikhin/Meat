open XHTML.M

module C = Css_main
module CR = C.Recipe

let of_part (text) n = <<
	<div class=$CR.Parts.part_div$>
		<div class=$CR.Parts.part_number$>$str:string_of_int n$.</div>
		$str:text$
	</div>
>>

let of_ingridient (text) n = <<
	<li class=$CR.Ingridients.ingridient_li$>
		$str:text$
	</li>
>>

let of_nlist f lst =
	let rec loop n = function
		| [] -> []
		| part :: tl -> f part n :: loop (n+1) tl
	in
	loop 1 lst

let of_parts = of_nlist of_part
let of_ingridients = of_nlist of_ingridient

let f ~ingridients_count ~ingridients ~parts name =
	let count = Int32.to_string ingridients_count in
	let r = Page_template.main ~js:"/js/js_recipe.js" <<
		<div class=$Css_main.Main.Content.container_div$>
			$str:name$ содержит $str:count$ ингридиентов.
			<ul class=$CR.Ingridients.container_ul$>
				$list:of_ingridients ingridients$
			</ul>
			<div class=$CR.Parts.container_div$>
				$list:of_parts parts$
			</div>
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
