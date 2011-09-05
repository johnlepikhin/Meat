
let bytes l =
	let s = String.create l in
	for i=0 to l-1 do
		s.[i] <- Char.chr(32 + (Random.int (127-32)))
	done;
	s

let _ =
	Random.self_init ()
