
type t = (string, bool) Hashtbl.t

let h : t PGOCaml.t = Lwt_unix.run (PGOCaml.connect ())
