open XHTML.M

let main ~sp (p_ingridients, p_props) =
	lwt f = D_search_form.search_form ~sp ~p_props in
	Page_template.main ~js:"/js/js_search.js" ~sp ~page_name:Common.PageName.main <<
		<div class=$Css_main.Main.Content.container_div$>
			$f$
		</div>
	>>
