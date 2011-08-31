
module type TABLE = sig
	type row
	type key
	type db_row

	val of_db: db_row -> key * row

	val all: unit -> db_row list Lwt.t

	val select: key -> db_row list Lwt.t

	val insert: row -> unit Lwt.t

	val delete: key -> unit Lwt.t

	val update: key -> row -> unit Lwt.t
end

module Cache
	= functor(Tbl : TABLE) ->
	struct
		type t = (Tbl.key, Tbl.row) Hashtbl.t

		let cache = ref None

		let fill () =
			lwt lst = Tbl.all () in
			let h = Hashtbl.create 103 in
			eachl lst (
				let (key, row) = Tbl.of_db __ in
				Hashtbl.add h key row;
			);
			cache := Some h;
			Lwt.return h

		let get () =
			match !cache with
				| None -> fill ()
				| Some c -> Lwt.return c

		let all () =
			lwt c = get () in
			let r = ref [] in
			Hashtbl.iter (fun k v -> r := v :: !r) c;
			Lwt.return !r

		let select key =
			lwt c = get () in
			try
				let row = Hashtbl.find c key in
				Lwt.return row
			with
				| _ ->
					Lwt.fail Not_found

		let insert key row =
			lwt c = get () in
			try_lwt
				lwt _ = Tbl.insert row in
				Hashtbl.add c key row;
				Lwt.return ()
			with
				| e ->
					Lwt.fail e

		let update key row =
			lwt c = get () in
			try_lwt
				lwt _ = Tbl.update key row in
				Hashtbl.replace c key row;
				Lwt.return ()
			with
				| e ->
					Lwt.fail e

		let delete key =
			lwt c = get () in
			try_lwt
				lwt _ = Tbl.delete key in
				Hashtbl.remove c key;
				Lwt.return ()
			with
				| e ->
					Lwt.fail e
	end

