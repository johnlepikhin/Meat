open XHTML.M

module C = Css_main
module CR = C.Recipe

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

let of_ingridients = of_nlist of_ingridient

let invalid_text req =
	Page_template.main req <<
		<div class=$Css_main.Main.Content.container_div$>
			Рецепт $str:(Processor.Page.get req)$ содержит ошибку, поэтому пока что мы его не показываем. Приносим свои извинения.
		</div>
	>>

let f ~ingridients_count ~ingridients ~text req =
	let title = Processor.Page.get req in
	let parts = Fr_parse.to_html text in
	match parts with
		| None -> invalid_text req
		| Some parts ->
			let parts = List.map L_HTML_XHTML.to_XHTML parts in
			Page_template.main req <<
				<div class=$Css_main.Main.Content.container_div$>
					<div class=$CR.title_div$ id=$Common.Recipe.title_id$>
						$str:title$
					</div>
					<ul class=$CR.Ingridients.container_ul$>
						$list:of_ingridients ingridients$
					</ul>
					<div class=$CR.edit_div$ id=$Common.Recipe.edit_div_id$/>
					<div class=$CR.Parts.container_div$ id=$Common.Recipe.container_div_id$>
						$list:parts$
					</div>
				</div>
			>>

let not_found req =
	Page_template.main req <<
		<div class=$Css_main.Main.Content.container_div$>
			Рецепт $str:(Processor.Page.get req)$ у нас еще не описан.
		</div>
	>>
