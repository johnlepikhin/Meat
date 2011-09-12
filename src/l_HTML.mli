module XML : sig
	type node = {
		name : L_HTML_Type.elname;
		attributes : L_HTML_Type._attribute list;
		children : t list;
	}

	and t =
		| Node of node
		| Text of string
end

type 'a t = XML.t

type 'a attribute

type ('attribute, 'child, 'result) unary = ?a:('attribute attribute list) -> 'child t -> 'result t
type ('attribute, 'child, 'result) star = ?a:('attribute attribute list) -> 'child t list -> 'result t
type ('attribute, 'child, 'result) plus = ?a:('attribute attribute list) -> 'child t -> 'child t list -> 'result t
type ('attribute, 'result) nullary = ?a:('attribute attribute list) -> unit -> 'result t

type core = [ `Class | `Id | `Title ]
type common = [ core | `Style_Attr]

type heading = [ `H1 | `H2 | `H3 | `H4 | `H5 | `H6 ]
type block = [ `Address | `Blockquote | `Div | `P | `Pre ]
type inline = [ `Abbr | `Acronym | `Br | `Cite | `Code | `Dfn | `Em | `Kbd | `Q | `Samp | `Span | `Strong | `Var | `Textarea ]
type flow = [ heading | block | inline ]


val a_class : string -> [< common ] attribute


val text: string -> [> `PCDATA ] t

val div : ([< common ], [< `PCDATA | flow ], [>`Div]) star

val img: alt : string -> src : string -> ([< common | `Height | `Longdesc | `Name | `Width | `Usemap |`Ismap ], [>`Img]) nullary

val textarea : rows:int -> cols:int -> string ->
	([< common | `Accesskey | `Name | `Tabindex | `Disabled | `Readonly |`OnBlur |`OnChange |`OnFocus | `OnSelect],
	 [>`Textarea]) nullary
