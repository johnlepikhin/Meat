
let f _ () () =
	let r = D_main.main () in
	Lwt.return r
