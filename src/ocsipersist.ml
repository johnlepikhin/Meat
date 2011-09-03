
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
	Ocsigen_messages.errlog ("get " ^ store ^ " " ^ name);
	lwt v = PGSQL(Db.h) "select value from ocsigen_storage where store=$store and name=$name" in
	match v with
		| [v] ->
			Ocsigen_messages.errlog ("done _get " ^ store ^ " " ^ name);
			return v
		| _ ->
			Ocsigen_messages.errlog ("error _get " ^ store ^ " " ^ name);
			fail Not_found

let insert id v =
	let store = id.store in
	let name = id.name in
	let v = Marshal.to_string v [] in
	Ocsigen_messages.errlog ("insert " ^ store ^ " " ^ name);
	PGSQL(Db.h) "insert into ocsigen_storage (store, name, value) values ($store, $name, $v)"

let update id v =
	let store = id.store in
	let name = id.name in
	let v = Marshal.to_string v [] in
	Ocsigen_messages.errlog ("update " ^ store ^ " " ^ name);
	PGSQL(Db.h) "update ocsigen_storage set value=$v where store=$store and name=$name"

let delete id =
	let store = id.store in
	let name = id.name in
	Ocsigen_messages.errlog ("delete " ^ store ^ " " ^ name);
	PGSQL(Db.h) "delete from ocsigen_storage where store=$store and name=$name"

let id store name = {
	store = store;
	name = name;
}

let make_persistent_lazy ~store ~name ~default =
Ocsigen_messages.errlog "make_persistent_lazy";
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
Ocsigen_messages.errlog "get";
	lwt v = _get id in
	let r = Marshal.from_string v 0 in
	return r

let set id v =
Ocsigen_messages.errlog "set";
	update id v

let db_iter_step table rowid =
Ocsigen_messages.errlog "db_iter_step";
	lwt v = PGSQL(Db.h) "select name, value, id from ocsigen_storage where id>$rowid" in
	match v with
		| [name, v, id] ->
			let v = Marshal.from_string v 0 in
			return (Some (name, v, id))
		| _ -> return None

let db_iter_block f table =
Ocsigen_messages.errlog "db_iter_block";
	lwt v = PGSQL(Db.h) "select name, value from ocsigen_storage where store=$table" in
	List.iter (fun (name, v) ->
		let v = Marshal.from_string v 0 in
		f name v
	) v;
	Lwt.return ()

let db_length table =
Ocsigen_messages.errlog "db_length";
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

let table_name table =
Ocsigen_messages.errlog "table_name";
 	return table

let find table key =
Ocsigen_messages.errlog "find";
	get (id table key)

let add table key v =
Ocsigen_messages.errlog "add";
	let id = id table key in
	lwt _ = delete id in
	insert id v

let replace_if_exists table key v =
Ocsigen_messages.errlog "replace_if_exists";
	let id = id table key in
	lwt _ = get id in
	set id v

let remove table key =
Ocsigen_messages.errlog "remove";
	delete (id table key)

let iter_step f table =
Ocsigen_messages.errlog "iter_step";
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
Ocsigen_messages.errlog "fold_step";
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
Ocsigen_messages.errlog "iter_block";
	db_iter_block

let iter_table =
Ocsigen_messages.errlog "iter_table";
	iter_step

let fold_table =
Ocsigen_messages.errlog "fold_table";
	fold_step

let length =
Ocsigen_messages.errlog "length";
	db_length

(**********************************************************)

let init _ =
	try
		ignore (Lwt_unix.run (PGSQL(Db.h) "select 1"));
	with e ->
		Ocsigen_messages.errlog "Error opening ocsipersist-postgres";
		raise e

let _ = Ocsigen_extensions.register_extension
	~name:"ocsipersist"
	~init_fun:init
	()
