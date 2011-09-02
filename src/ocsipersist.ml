
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
	lwt v = PGSQL(Db.h) "select value from ocsigen_storage where store=$store and name=$name" in
	match v with
		| [v] -> return v
		| _ -> fail Not_found

let insert id v =
	let store = id.store in
	let name = id.name in
	let v = Marshal.to_string v [] in
	PGSQL(Db.h) "insert into ocsigen_storage (store, name, value) values ($store, $name, $v)"

let update id v =
	let store = id.store in
	let name = id.name in
	let v = Marshal.to_string v [] in
	PGSQL(Db.h) "update ocsigen_storage set value=$v where store=$store and name=$name"

let delete id =
	let store = id.store in
	let name = id.name in
	PGSQL(Db.h) "delete from ocsigen_storage where store=$store and name=$name"

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
	lwt v = PGSQL(Db.h) "select name, value, id from ocsigen_storage where id>$rowid" in
	match v with
		| [name, v, id] ->
			let v = Marshal.from_string v 0 in
			return (Some (name, v, id))
		| _ -> return None

let db_iter_block f table =
	lwt v = PGSQL(Db.h) "select name, value from ocsigen_storage where store=$table" in
	List.iter (fun (name, v) ->
		let v = Marshal.from_string v 0 in
		f name v
	) v;
	Lwt.return ()

let db_length table =
	lwt v = PGSQL(Db.h) "select count(*) from ocsigen_storage where store=$table" in
	match v with
		| [Some v] ->
			let v = Int64.to_int v in
			return v
		| _ -> fail Not_found

(**********************************************************)

(** Type of persistent tables *)
type 'value table = string

(** name SHOULD NOT begin with "store___" *)
let open_table name = name

let table_name table = return table

let find table key = get (id table key)

let add table key = insert (id table key)

let replace_if_exists table key = set (id table key)

let remove table key = delete (id table key)

let iter_step f table =
	let rec aux rowid =
		lwt r = db_iter_step table rowid in
		match r with
			| None -> return ()
			| Some (k, v, new_rowid) ->
				lwt _ = f k (Marshal.from_string v 0) in
				aux new_rowid
	in
	aux Int64.zero

let fold_step f table beg =
	let rec aux rowid beg =
		lwt r = db_iter_step table rowid in
		match r with
			| None -> return beg
			| Some (k, v, new_rowid) ->
				lwt res = f k (Marshal.from_string v 0) beg in
				aux new_rowid res
	in
	aux Int64.zero beg

let iter_block = db_iter_block

let iter_table = iter_step

let fold_table = fold_step

let length = db_length

(**********************************************************)

let init _ =
	try
		ignore (Lwt_unix.run (PGSQL(Db.h) "select 1"))
	with e ->
		Ocsigen_messages.errlog "Error opening ocsipersist-postgres";
		raise e

let _ = Ocsigen_extensions.register_extension
	~name:"ocsipersist"
	~init_fun:init
	()
