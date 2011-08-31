
module PG_LWT = PGOCaml_generic.Make(struct include Lwt include Lwt_chan end)

include PG_LWT
