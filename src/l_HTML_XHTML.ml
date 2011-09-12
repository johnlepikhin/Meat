open L_HTML_Type

open L_HTML
open XML
open XHTML.M

let attribute_to_XHTML = function
	| Id v -> a_id v
	| Class v -> a_class [v]

let rec el_to_XHTML children a = function
	| EDiv -> div ~a children
	| EImg (alt, src) -> img ~alt ~src:(uri_of_string src) ~a ()
	| ETextArea (rows, cols, content) -> textarea ~rows ~cols ~a (pcdata content)

and to_XHTML = function
	| Node n ->
		let a = List.map attribute_to_XHTML n.attributes in
		let children = List.map to_XHTML n.children in
		el_to_XHTML children a n.name
	| Text t -> pcdata t
