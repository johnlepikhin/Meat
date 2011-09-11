open L_HTML_Type

module XML = struct
	type node = {
		name : elname;
		attributes : _attribute list;
		children : t list;
	}

	and t =
		| Node of node
		| Text of string

	let node name ?(a=[]) children = Node {
		name = name;
		attributes = a;
		children = children;
	}

	let text s = Text s
end

type 'a t = XML.t

type 'a attribute = _attribute

type ('attribute, 'child, 'result) star = ?a:('attribute attribute list) -> 'child t list -> 'result t
type ('attribute, 'child, 'result) plus = ?a:('attribute attribute list) -> 'child t -> 'child t list -> 'result t
type ('attribute, 'result) nullary = ?a:('attribute attribute list) -> unit -> 'result t

type core = [ `Class | `Id | `Title ]
type common = [ core | `Style_Attr]

type heading = [ `H1 | `H2 | `H3 | `H4 | `H5 | `H6 ]
type block = [ `Address | `Blockquote | `Div | `P | `Pre ]
type inline = [ `Abbr | `Acronym | `Br | `Cite | `Code | `Dfn | `Em | `Kbd | `Q | `Samp | `Span | `Strong | `Var ]
type flow = [ heading | block | inline ]



let unary name ?a child = XML.node name ?a [child]

let plus name ?a child children = XML.node name ?a (child :: children)

let leaf name ?a () = XML.node name ?a []


let div = XML.node EDiv

let img ~alt ~src = leaf (EImg (alt, src))

let text t = XML.text t
