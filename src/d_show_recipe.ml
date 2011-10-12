open XHTML.M

module C = Css_main
module CR = C.Recipe

let invalid_text req =
	<<
		<div class=$Css_main.Main.Content.container_div$ id=$Common.Recipe.container_div_id$>
			Рецепт "$str:(Processor.Xhtml.get req)$" содержит ошибку, поэтому мы его не показываем. Приносим свои извинения.
		</div>
	>>

let f ~ingridients_count ~ingridients ~text req =
	let title = Processor.Xhtml.get req in
	let content = Fr_parse.to_html text in
	let content = match content with
		| None -> invalid_text req
		| Some content -> L_HTML_XHTML.to_XHTML content
	in
	Page_template.main req <<
		<div class=$Css_main.Main.Content.container_div$>
			$content$
			<div id=$Common.Recipe.edit_div_id$/>
		</div>
	>>

let not_found req =
	Page_template.main req <<
		<div class=$Css_main.Main.Content.container_div$>
			Рецепт $str:(Processor.Xhtml.get req)$ у нас еще не описан.
		</div>
	>>
