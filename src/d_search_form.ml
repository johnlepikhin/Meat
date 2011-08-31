open XHTML.M
open Eliom_predefmod.Xhtml

module CF = Css_main.Search.Form
module CR = Css_main.Search.Results
module FS = Common.Search

let prop_list ~properties_field p_props =
	lwt all_props = Tc.Property.all () in
	let r = mapl all_props (
		let i = string_checkbox
			~name:properties_field
			~a:[a_class [CF.Properties.checkbox]]
			~value:__.Table.Property.name
			~checked:(List.mem __.Table.Property.name p_props)
			()
		in
		<<
			<div class=$CF.Properties.element_div$>
				<label>
					$i$ $str:__.Table.Property.name$
				</label>
			</div>
		>>
	) in
	Lwt.return r

let search_form ~sp ~p_props =
	let generator (_, properties_field) =
		lwt prop_list = prop_list ~properties_field p_props in
		Lwt.return <:xmllist<
			<div class=$CF.container_div$>
				<div class=$CF.form_div$>
					<table class=$CF.table$>
						<tr>
							<td class=$CF.td_ingridients$>
								<input autocomplete="off" id=$FS.ingridient_id$ type="string" class=$CF.Ingridients.input$/>
								<div id=$FS.autocomplete_id$ class=$CF.Ingridients.Autocomplete.container_div_empty$/>
								<div id=$FS.selected_ingridients_list_id$ class=$CF.Selection.container_div_empty$/>
	
								<div id=$FS.selected_ingridients_inputs_id$/>
							</td>
							<td>
								$list:prop_list$
							</td>
							<td>
								<input type="submit" value="Поиск" class=$CF.submit_button$/>
							</td>
						</tr>
					</table>
				</div>
			</div>
		>>
	in
	lwt_get_form
		~a:[a_id FS.form_id]
		~sp
 		~service:Services.search_results
		generator
