
type t = (string, bool) Hashtbl.t PGOCaml.t

let pool : t Lwt_pool.t =
	Lwt_pool.create 10 PGOCaml.connect

let use (f : t -> 'a) : 'a = Lwt_pool.use pool f
