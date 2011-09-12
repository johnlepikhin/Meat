open XHTML.M
open Eliom_predefmod.Xhtml

module CR = Css_main.Search.Results

let search_results req results =
	let id = ref 0 in
	let id_list = ref [] in
	let r = List.map
		(fun (name, ingridient_count) ->
		 	incr id;
			id_list := !id :: !id_list;
		 	let link = a Services.show_recipe (Processor.Page.sp req) [pcdata name] name in
		 	<<
				<tr class=$CR.element_tr$>
					<td class=$CR.name_td$>
						$link$
					</td>
					<td class=$CR.info_td$>
						Необходимо ингридиентов: $str:Int32.to_string ingridient_count$
					</td>
				</tr>
			>>
		)
		results
	in
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
