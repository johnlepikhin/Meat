open XHTML.M
open Eliom_predefmod.Xhtml

module CR = Css_main.Search.Results
module C = Common.Search.Results

let search_results req results =
	let id = ref 0 in
	let id_list = ref [] in
	let r = List.map
		(fun (name, ingridient_count) ->
		 	incr id;
			id_list := !id :: !id_list;
		 	let link = a Services.show_recipe (Processor.Page.sp req) [pcdata name] name in
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
	Processor.Page.add_js_var req Common.Search.Results.ids_list_var (API.to_string !id_list);
	<<
		<table class=$CR.container_table$>
			$list:r$
		</table>
	>>

let main req results =
	let (selected_ingridients, selected_properties) = Processor.Page.get req in
	lwt f = D_search_form.search_form ~selected_ingridients ~selected_properties req in
	let results = search_results req results in
	lwt r = Page_template.main req <<
		<div class=$Css_main.Main.Content.container_div$>
			$f$
			$results$
		</div>
	>> in
	Lwt.return r
