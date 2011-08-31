open XHTML.M

let main ~sp (p_ingridients, p_props) =
	lwt f = D_search_form.search_form ~sp ~p_props in
	let r = Page_template.main ~js:"/js/js_search.js" <<
		<div class=$Css_main.Main.Content.container_div$>
			$f$
		</div>
	>> in
	Lwt.return r
