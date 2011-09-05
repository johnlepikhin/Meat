
open Lwt

type store = string

type 'a t = {
	store : store;
	name : string;
}

let open_store name = name

let _get id =
	let store = id.store in
	let name = id.name in
	Db.use (fun db ->
		lwt v = PGSQL(db) "select value from ocsigen_storage where store=$store and name=$name" in
		match v with
			| [v] ->
				return v
			| _ ->
				fail Not_found
	)

let insert id v =
	let store = id.store in
	let name = id.name in
	let v = Marshal.to_string v [] in
	Db.use (fun db -> PGSQL(db) "insert into ocsigen_storage (store, name, value) values ($store, $name, $v)")

let update id v =
	let store = id.store in
	let name = id.name in
	let v = Marshal.to_string v [] in
	Db.use (fun db -> PGSQL(db) "update ocsigen_storage set value=$v where store=$store and name=$name")

let delete id =
	let store = id.store in
	let name = id.name in
	Db.use (fun db -> PGSQL(db) "delete from ocsigen_storage where store=$store and name=$name")

let id store name = {
	store = store;
	name = name;
}

let make_persistent_lazy ~store ~name ~default =
	let id = id store name in
	try_lwt
		lwt _ = _get id in
		return id
	with
		| Not_found ->
			let v = default () in
			lwt _ = insert id v in
			return id
		| e ->
			fail e

let make_persistent ~store ~name ~default = make_persistent_lazy ~store ~name ~default:(fun () -> default)

let get id =
	lwt v = _get id in
	let r = Marshal.from_string v 0 in
	return r

let set id v =
	update id v

let db_iter_step table rowid =
	Db.use (fun db ->
		lwt v = PGSQL(db) "select name, value, id from ocsigen_storage where id>$rowid" in
		match v with
			| [name, v, id] ->
				let v = Marshal.from_string v 0 in
				return (Some (name, v, id))
			| _ -> return None
	)

let db_iter_block f table =
	Db.use (fun db ->
		lwt v = PGSQL(db) "select name, value from ocsigen_storage where store=$table" in
		List.iter (fun (name, v) ->
			let v = Marshal.from_string v 0 in
			f name v
		) v;
		Lwt.return ()
	)

let db_length table =
	Db.use (fun db ->
		lwt v = PGSQL(db) "select count(*) from ocsigen_storage where store=$table" in
		match v with
			| [Some v] ->
				let v = Int64.to_int v in
				return v
			| _ -> fail Not_found
	)

(**********************************************************)

(** Type of persistent tables *)
type 'value table = string

(** name SHOULD NOT begin with "store___" *)
let open_table name = name

let table_name table =
 	return table

let find table key =
	get (id table key)

let add table key v =
	let id = id table key in
	lwt _ = delete id in
	insert id v

let replace_if_exists table key v =
	let id = id table key in
	lwt _ = get id in
	set id v

let remove table key =
	delete (id table key)

let iter_step f table =
	let rec aux rowid =
		lwt r = db_iter_step table rowid in
		match r with
			| None -> return ()
			| Some (k, v, new_rowid) ->
				lwt _ = f k v in
				aux new_rowid
	in
	aux Int64.zero

let fold_step f table beg =
	let rec aux rowid beg =
		lwt r = db_iter_step table rowid in
		match r with
			| None -> return beg
			| Some (k, v, new_rowid) ->
				lwt res = f k v beg in
				aux new_rowid res
	in
	aux Int64.zero beg

let iter_block =
	db_iter_block

let iter_table =
	iter_step

let fold_table =
	fold_step

let length =
	db_length

(**********************************************************)

let check () =
	Db.use (fun db -> PGSQL(db) "select 1")

let init _ =
	try
		ignore (Lwt_unix.run (check ()))
	with e ->
		raise e

let _ = Ocsigen_extensions.register_extension
	~name:"ocsipersist"
	~init_fun:init
	()
