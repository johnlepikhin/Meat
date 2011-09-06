open XHTML.M
open Eliom_predefmod.Xhtml

module CR = Css_main.Search.Results
module C = Common

let search_results ~sp results =
	let id = ref 0 in
	let r = List.map
		(fun (name, ingridient_count) ->
		 	incr id;
		 	let link = a Services.show_recipe sp [pcdata name] name in
		 	<<
				<div class=$CR.element_div$ id=$C.Results.element_div_prefix ^ (string_of_int !id)$>
					<div class=$CR.name_div$>
						$link$
					</div>
					<div class=$CR.info_div$ onclick=$str:C.Results.show_ingridients_f ^ "(\"" ^ name ^ "\")"$>
						Необходимо ингридиентов: $str:Int32.to_string ingridient_count$
					</div>
					<div class=$CR.delete_div$ id=$C.Results.element_delete_div_prefix ^ (string_of_int !id)$>
						X
					</div>
				</div>
			>>
		)
		results
	in
	<<
		<div class=$CR.container_div$>
			$list:r$
		</div>
	>>

let main ~sp (p_ingridients, p_props) results =
	lwt f = D_search_form.search_form ~sp ~p_props in
	let results = search_results ~sp results in
	lwt r = Page_template.main ~js:"/js/js_search.js" ~sp ~page_type:API.Page.SearchResults <<
		<div class=$Css_main.Main.Content.container_div$>
			$f$
			$results$
		</div>
	>> in
	Lwt.return r
