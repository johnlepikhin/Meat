
let f _ query () =
	Lwt.return (D_search_results.main query)
