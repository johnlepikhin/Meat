
open Js

let now () =
	let now = jsnew date_now () in
	(to_float (now##getTime ())) /. 1000.
