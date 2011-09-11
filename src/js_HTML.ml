open L_HTML_Type

open L_HTML
open XML
open Dom_html
open Js_primitives
open Js

let attribute_to_JS el = function
	| Id v -> el##id <- string v

let rec el_to_JS = function
	| EDiv ->
		let div = createDiv doc in
		Js_common.Coerce.coerce div Dom_html.CoerceTo.element
	| EImg (alt, src) ->
		let img = createImg doc in
		img##alt <- string alt;
		img##src <- string src;
		Js_common.Coerce.coerce img Dom_html.CoerceTo.element

and to_JS parent = function
	| Node n ->
		lwt el = el_to_JS n.name in
		lwt _ = iter_list el n.children in
		List.iter (attribute_to_JS el) n.attributes;
		Dom.appendChild parent el;
		Lwt.return ()
	| Text t ->
		let pcdata = doc##createTextNode (string t) in
		Dom.appendChild parent pcdata;
		Lwt.return ()

and iter_list parent = function
	| [] -> Lwt.return ()
	| child :: tl ->
		lwt child = to_JS parent child in
		iter_list parent tl
