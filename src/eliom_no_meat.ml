
open Eliom_predefmod

let xhtml service f =
	Xhtml.register ~service f

let main = xhtml Services.main Pg_main.f
