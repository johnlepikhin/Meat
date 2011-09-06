open XHTML.M
open Eliom_predefmod.Xhtml

module CR = Css_main.Search.Results
module C = Common.Search.Results

let search_results ~sp results =
	let id = ref 0 in
	let id_list = ref [] in
	let r = List.map
		(fun (name, ingridient_count) ->
		 	incr id;
			id_list := !id :: !id_list;
		 	let link = a Services.show_recipe sp [pcdata name] name in
		 	<<
				<tr class=$CR.element_tr$ id=$C.element_tr_prefix ^ (string_of_int !id)$>
					<td class=$CR.name_td$>
						$link$
					</td>
					<td class=$CR.info_td$>
						Необходимо ингридиентов: $str:Int32.to_string ingridient_count$
					</td>
					<td class=$CR.delete_td$>
						<div class=$CR.delete_div$ id=$C.element_delete_div_prefix ^ (string_of_int !id)$>X</div>
					</td>
				</tr>
			>>
		)
		results
	in
	let xhtml = <<
		<table class=$CR.container_table$>
			$list:r$
		</table>
	>> in
	xhtml, (Common.Search.Results.ids_list_var, API.to_string !id_list)

let main ~sp (p_ingridients, p_props) results =
	lwt f = D_search_form.search_form ~sp ~p_props in
	let (results, ids_list) = search_results ~sp results in
	lwt r = Page_template.main ~js:"/js/js_search.js" ~sp ~page_type:API.Page.SearchResults ~js_vars:[ids_list] <<
		<div class=$Css_main.Main.Content.container_div$>
			$f$
			$results$
		</div>
	>> in
	Lwt.return r
