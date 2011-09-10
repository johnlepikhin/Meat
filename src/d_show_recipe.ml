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

let f ~ingridients_count ~ingridients ~parts req =
	let title = Processor.Page.get req in
	let info = {
		API.ShowRecipe.title = title;
		API.ShowRecipe.parts = [];
	} in
	Processor.Page.add_js_var req Common.Recipe.info_var (API.to_string info);
	Page_template.main req <<
		<div class=$Css_main.Main.Content.container_div$>
			<div class=$CR.title_div$ id=$Common.Recipe.title_id$>
				$str:title$
			</div>
			<ul class=$CR.Ingridients.container_ul$>
				$list:of_ingridients ingridients$
			</ul>
			<div class=$CR.Parts.container_div$>
				$list:of_parts parts$
			</div>
		</div>
	>>

let not_found req =
	Page_template.main req <<
		<div class=$Css_main.Main.Content.container_div$>
			Рецепт $str:(Processor.Page.get req)$ у нас еще не описан.
		</div>
	>>
