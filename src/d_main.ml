open XHTML.M

let main req =
	lwt f = D_search_form.search_form ~selected_ingridients:[] ~selected_properties:[] req in
	Page_template.main req <<
		<div class=$Css_main.Main.Content.container_div$>
			$f$
		</div>
	>>
